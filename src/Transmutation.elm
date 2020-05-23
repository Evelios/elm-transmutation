module Transmutation exposing
    ( Transmutation
    , getGeometry
    , cross, vertexFork, midpointInset, midpointInsetAndFork
    )

{-|


# Types

@docs Transmutation


# Accessors

@docs getGeometry


# Transmutation functions

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
    = Terminal (List (Geometry units coordinates))
    | Fork (List (Geometry units coordinates)) (List (Transmutation units coordinates))
    | Internal (List (Geometry units coordinates)) (Transmutation units coordinates)
    | ForkWithInternal (List (Geometry units coordinates)) (List (Transmutation units coordinates)) (Transmutation units coordinates)



-------- Building


terminal : List (Geometry units coordinates) -> Transmutation units coordinates
terminal =
    Terminal


fork :
    { geometry : List (Geometry units coordinates)
    , external : List (Transmutation units coordinates)
    }
    -> Transmutation units coordinates
fork { geometry, external } =
    Fork geometry external


internal :
    { geometry : List (Geometry units coordinates)
    , internalTransmutation : Transmutation units coordinates
    }
    -> Transmutation units coordinates
internal { geometry, internalTransmutation } =
    Internal geometry internalTransmutation


forkWithInternal :
    { geometry : List (Geometry units coordinates)
    , externalTransmutations : List (Transmutation units coordinates)
    , internalTransmutation : Transmutation units coordinates
    }
    -> Transmutation units coordinates
forkWithInternal { geometry, externalTransmutations, internalTransmutation } =
    ForkWithInternal geometry externalTransmutations internalTransmutation



-------- Accessors


{-| Get the geometry from this level of the transmutation.
-}
getGeometry : Transmutation units coordinates -> List (Geometry units coordinates)
getGeometry transmutation =
    case transmutation of
        Terminal geometry ->
            geometry

        Fork geometry forkTransmutations ->
            List.append geometry <| List.concat <| List.map getGeometry forkTransmutations

        Internal geometry internalTransmutation ->
            List.append geometry <| getGeometry internalTransmutation

        ForkWithInternal geometry externalTransmutations internalTransmutation ->
            geometry
                |> List.append (List.concat (List.map getGeometry externalTransmutations))
                |> List.append (getGeometry internalTransmutation)



-------- Types


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
    fork
        { external = List.map cross continuations
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
    internal
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
    forkWithInternal
        { externalTransmutations = List.map cross forks
        , internalTransmutation = cross rotatedInset
        , geometry = geometry
        }
