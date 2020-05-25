module Transmutation exposing
    ( Transmutation
    , getGeometry
    , terminal, forkWith, internalWith, forkAndInternalWith
    , all, allForks, allInternals, allForksWithInternals
    , cross, vertexFork, midpointInset, midpointInsetAndFork
    )

{-|


# Types

@docs Transmutation


# Accessors

@docs getGeometry


# Modifiers

@docs terminal, forkWith, internalWith, forkAndInternalWith


# Transmutation functions

@docs all, allForks, allInternals, allForksWithInternals

@docs cross, vertexFork, midpointInset, midpointInsetAndFork

-}

import Geometry exposing (Geometry(..))
import LineSegment2d
import List.Extra
import List.Util
import Quantity
import RegularPolygon2d exposing (RegularPolygon2d)


{-| -}
type Transmutation units coordinates
    = Terminal
        { geometry : List (Geometry units coordinates)
        }
    | Internal
        { geometry : List (Geometry units coordinates)
        , internalTransmutation : Transmutation units coordinates
        }
    | Fork
        { geometry : List (Geometry units coordinates)
        , forkTransmutations : List (Transmutation units coordinates)
        }
    | ForkWithInternal
        { geometry : List (Geometry units coordinates)
        , forkTransmutations : List (Transmutation units coordinates)
        , internalTransmutation : Transmutation units coordinates
        }



-------- Building


terminal : List (Geometry units coordinates) -> Transmutation units coordinates
terminal geometry =
    Terminal { geometry = geometry }


forkWith :
    { geometry : List (Geometry units coordinates)
    , forkTransmutations : List (Transmutation units coordinates)
    }
    -> Transmutation units coordinates
forkWith =
    Fork


internalWith :
    { geometry : List (Geometry units coordinates)
    , internalTransmutation : Transmutation units coordinates
    }
    -> Transmutation units coordinates
internalWith =
    Internal


forkAndInternalWith :
    { geometry : List (Geometry units coordinates)
    , forkTransmutations : List (Transmutation units coordinates)
    , internalTransmutation : Transmutation units coordinates
    }
    -> Transmutation units coordinates
forkAndInternalWith =
    ForkWithInternal



-------- Accessors


{-| Get the geometry from this level of the transmutation.
-}
getGeometry : Transmutation units coordinates -> List (Geometry units coordinates)
getGeometry transmutation =
    case transmutation of
        Terminal { geometry } ->
            geometry

        Fork { geometry, forkTransmutations } ->
            List.append geometry <| List.concat <| List.map getGeometry forkTransmutations

        Internal { geometry, internalTransmutation } ->
            List.append geometry <| getGeometry internalTransmutation

        ForkWithInternal { geometry, forkTransmutations, internalTransmutation } ->
            geometry
                |> List.append (List.concat (List.map getGeometry forkTransmutations))
                |> List.append (getGeometry internalTransmutation)



-------- Transmutations


all : List (RegularPolygon2d units coordinates -> Transmutation units coordinates)
all =
    allTerminals
        |> List.append allForks
        |> List.append allInternals
        |> List.append allForksWithInternals


allTerminals : List (RegularPolygon2d units coordinates -> Transmutation units coordinates)
allTerminals =
    [ cross
    ]


allForks : List (RegularPolygon2d units coordinates -> Transmutation units coordinates)
allForks =
    [ vertexFork
    ]


allInternals : List (RegularPolygon2d units coordinates -> Transmutation units coordinates)
allInternals =
    [ midpointInset
    ]


allForksWithInternals : List (RegularPolygon2d units coordinates -> Transmutation units coordinates)
allForksWithInternals =
    [ midpointInsetAndFork
    ]


cross : RegularPolygon2d units coordinates -> Transmutation units coordinates
cross polygon =
    let
        midpoints =
            RegularPolygon2d.midpoints polygon
                |> List.Util.rotateLeft (RegularPolygon2d.sides polygon // 2)

        internalGeometry =
            List.Extra.zip (RegularPolygon2d.vertices polygon) midpoints
                |> List.map LineSegment2d.fromEndpoints
                |> List.map Line
    in
    terminal internalGeometry


vertexFork : RegularPolygon2d units coordinates -> Transmutation units coordinates
vertexFork polygon =
    let
        createPolygon vertex =
            RegularPolygon2d.fromUnsafe
                { sides = RegularPolygon2d.sides polygon
                , radius = Quantity.half (RegularPolygon2d.edgeLength polygon)
                , center = vertex
                , angle = RegularPolygon2d.angle polygon
                }

        continuations =
            RegularPolygon2d.vertices polygon
                |> List.map createPolygon
    in
    forkWith
        { forkTransmutations = List.map cross continuations
        , geometry = List.map RegularPolygon continuations
        }


midpointInset : RegularPolygon2d units coordinates -> Transmutation units coordinates
midpointInset polygon =
    let
        rotatedInset =
            polygon
                |> RegularPolygon2d.withRadius (RegularPolygon2d.internalRadius polygon)
                |> RegularPolygon2d.rotateHalfExteriorAngle
    in
    internalWith
        { geometry = [ RegularPolygon rotatedInset ]
        , internalTransmutation = cross rotatedInset
        }


midpointInsetAndFork : RegularPolygon2d units coordinates -> Transmutation units coordinates
midpointInsetAndFork polygon =
    let
        rotatedInset =
            polygon
                |> RegularPolygon2d.withRadius (RegularPolygon2d.internalRadius polygon)
                |> RegularPolygon2d.rotateHalfExteriorAngle

        forkRadius =
            RegularPolygon2d.radius polygon |> Quantity.minus (RegularPolygon2d.internalRadius rotatedInset)

        forkPolygon vertex =
            RegularPolygon2d.fromUnsafe
                { sides = RegularPolygon2d.sides polygon
                , radius = forkRadius
                , center = vertex
                , angle = RegularPolygon2d.angle (RegularPolygon2d.rotateHalfExteriorAngle polygon)
                }

        forks =
            RegularPolygon2d.vertices polygon
                |> List.map forkPolygon

        geometry =
            RegularPolygon rotatedInset :: List.map RegularPolygon forks
    in
    forkAndInternalWith
        { forkTransmutations = []
        , internalTransmutation = cross rotatedInset
        , geometry = geometry
        }
