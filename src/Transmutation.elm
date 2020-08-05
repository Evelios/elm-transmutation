module Transmutation exposing
    ( Transmutation
    , terminal, forkWith, internalWith, forkAndInternalWith
    , withInternal, withFork
    , getGeometry
    , all, allForks, allInternals, allForksWithInternals
    , cross, vertexFork, midpointInset, midpointInsetAndFork
    )

{-|


# Types

@docs Transmutation


# Builders

@docs terminal, forkWith, internalWith, forkAndInternalWith


# Modifiers

@docs withInternal, withFork, normalize


# Accessors

@docs getGeometry


# Transmutation functions

@docs all, allForks, allInternals, allForksWithInternals

@docs cross, vertexFork, midpointInset, midpointInsetAndFork

-}

import BoundingBox2d exposing (BoundingBox2d)
import Geometry exposing (Geometry(..))
import LineSegment2d
import List.Extra
import List.Util
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import RegularPolygon2d exposing (RegularPolygon2d)


type alias Model units coordinates a =
    { a
        | geometry : List (Geometry units coordinates)
    }


{-| -}
type Transmutation units coordinates
    = Terminal (Model units coordinates {})
    | Internal
        (Model units
            coordinates
            { internalContinuation : RegularPolygon2d units coordinates
            , internalTransmutation : Maybe (Transmutation units coordinates)
            }
        )
    | Fork
        (Model units
            coordinates
            { forkContinuations : List (RegularPolygon2d units coordinates)
            , forkTransmutations : List (Transmutation units coordinates)
            }
        )
    | ForkWithInternal
        (Model units
            coordinates
            { internalContinuation : RegularPolygon2d units coordinates
            , internalTransmutation : Maybe (Transmutation units coordinates)
            , forkContinuations : List (RegularPolygon2d units coordinates)
            , forkTransmutations : List (Transmutation units coordinates)
            }
        )



-------- Builders


terminal : List (Geometry units coordinates) -> Transmutation units coordinates
terminal geometry =
    Terminal { geometry = geometry }


forkWith :
    { geometry : List (Geometry units coordinates)
    , forkContinuations : List (RegularPolygon2d units coordinates)
    }
    -> Transmutation units coordinates
forkWith { geometry, forkContinuations } =
    Fork
        { geometry = geometry
        , forkContinuations = forkContinuations
        , forkTransmutations = []
        }


internalWith :
    { geometry : List (Geometry units coordinates)
    , internalContinuation : RegularPolygon2d units coordinates
    }
    -> Transmutation units coordinates
internalWith { geometry, internalContinuation } =
    Internal
        { geometry = geometry
        , internalContinuation = internalContinuation
        , internalTransmutation = Nothing
        }


forkAndInternalWith :
    { geometry : List (Geometry units coordinates)
    , forkContinuations : List (RegularPolygon2d units coordinates)
    , internalContinuation : RegularPolygon2d units coordinates
    }
    -> Transmutation units coordinates
forkAndInternalWith { geometry, forkContinuations, internalContinuation } =
    ForkWithInternal
        { geometry = geometry
        , internalContinuation = internalContinuation
        , internalTransmutation = Nothing
        , forkContinuations = forkContinuations
        , forkTransmutations = []
        }



-------- Modifiers


withInternal :
    (RegularPolygon2d units coordinates -> Transmutation units coordinates)
    -> Transmutation units coordinates
    -> Transmutation units coordinates
withInternal algorithm transmutation =
    case transmutation of
        Terminal _ ->
            transmutation

        Internal records ->
            Internal { records | internalTransmutation = Just (algorithm records.internalContinuation) }

        Fork _ ->
            transmutation

        ForkWithInternal records ->
            ForkWithInternal { records | internalTransmutation = Just (algorithm records.internalContinuation) }


withFork :
    (RegularPolygon2d units coordinates -> Transmutation units coordinates)
    -> Transmutation units coordinates
    -> Transmutation units coordinates
withFork algorithm transmutation =
    case transmutation of
        Terminal _ ->
            transmutation

        Internal _ ->
            transmutation

        Fork records ->
            Fork { records | forkTransmutations = List.map algorithm records.forkContinuations }

        ForkWithInternal records ->
            ForkWithInternal { records | forkTransmutations = List.map algorithm records.forkContinuations }


{-| Apply a function to the geometry of each element within the transmutation.
-}
apply :
    (Geometry units coordinates -> Geometry units coordinates)
    -> Transmutation units coordinates
    -> Transmutation units coordinates
apply modification transmutation =
    case transmutation of
        Terminal model ->
            Terminal { model | geometry = List.map modification model.geometry }

        Internal model ->
            Internal
                { model
                    | geometry = List.map modification model.geometry
                    , internalTransmutation = Maybe.map (apply modification) model.internalTransmutation
                }

        Fork model ->
            Fork
                { model
                    | geometry = List.map modification model.geometry
                    , forkTransmutations = List.map (apply modification) model.forkTransmutations
                }

        ForkWithInternal model ->
            ForkWithInternal
                { model
                    | geometry = List.map modification model.geometry
                    , internalTransmutation = Maybe.map (apply modification) model.internalTransmutation
                    , forkTransmutations = List.map (apply modification) model.forkTransmutations
                }


{-| Scale the transmutation by a particular amount
-}
scaleAbout : Point2d units coordinates -> Float -> Transmutation units coordinates -> Transmutation units coordinates
scaleAbout point amount =
    apply <| Geometry.scaleAbout point amount



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
            case internalTransmutation of
                Just internal ->
                    List.append geometry <| getGeometry internal

                Nothing ->
                    geometry

        ForkWithInternal { geometry, forkTransmutations, internalTransmutation } ->
            case internalTransmutation of
                Just internal ->
                    geometry
                        |> List.append (List.concat (List.map getGeometry forkTransmutations))
                        |> List.append (getGeometry internal)

                Nothing ->
                    geometry
                        |> List.append (List.concat (List.map getGeometry forkTransmutations))


boundingBox : Transmutation units coordinates -> BoundingBox2d units coordinates
boundingBox transmutation =
    case getGeometry transmutation of
        first :: rest ->
            BoundingBox2d.aggregateOf Geometry.boundingBox first rest

        [] ->
            BoundingBox2d.singleton Point2d.origin


center : Transmutation units coordinates -> Point2d units coordinates
center transmutation =
    BoundingBox2d.centerPoint <| boundingBox transmutation



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
        { forkContinuations = continuations
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
        , internalContinuation = rotatedInset
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
        { forkContinuations = forks
        , internalContinuation = rotatedInset
        , geometry = geometry
        }
