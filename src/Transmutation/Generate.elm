module Transmutation.Generate exposing (..)

import Random exposing (Generator)
import RegularPolygon2d exposing (RegularPolygon2d)
import Transmutation exposing (Transmutation)


transmutation :
    { startingAlgorithm : RegularPolygon2d units coordinates -> Transmutation units coordinates
    , algorithms : List (RegularPolygon2d units coordinates -> Transmutation units coordinates)
    , startingPolygon : RegularPolygon2d units coordinates
    , terminalCondition : RegularPolygon2d units coordinates -> Bool
    }
    -> Generator (Transmutation units coordinates)
transmutation { startingAlgorithm, algorithms, startingPolygon, terminalCondition } =
    Random.constant <| startingAlgorithm startingPolygon
