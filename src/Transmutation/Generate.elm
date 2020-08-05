module Transmutation.Generate exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Unitless)
import Random exposing (Generator)
import RegularPolygon2d exposing (RegularPolygon2d)
import Transmutation exposing (Transmutation)


type alias TransmutationSettings units coordinates =
    { startingAlgorithm : RegularPolygon2d units coordinates -> Transmutation units coordinates
    , algorithms : List (RegularPolygon2d units coordinates -> Transmutation units coordinates)
    , startingPolygon : RegularPolygon2d units coordinates
    , terminalCondition : RegularPolygon2d units coordinates -> Bool
    }


transmutation : TransmutationSettings units coordinates -> Generator (Transmutation units coordinates)
transmutation { startingAlgorithm, algorithms, startingPolygon, terminalCondition } =
    Random.constant (startingAlgorithm startingPolygon)


transmutationDefault : TransmutationSettings Unitless coordinates
transmutationDefault =
    let
        startingPolygon =
            RegularPolygon2d.fromUnsafe
                { sides = 5
                , radius = Quantity.float 1
                , center = Point2d.origin
                , angle = Quantity.zero
                }
    in
    { startingAlgorithm = Transmutation.midpointInsetAndFork
    , algorithms = [ Transmutation.cross ]
    , startingPolygon = startingPolygon
    , terminalCondition = \_ -> True
    }
