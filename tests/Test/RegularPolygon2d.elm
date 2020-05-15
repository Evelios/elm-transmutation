module Test.RegularPolygon2d exposing (..)

import Angle
import Expect exposing (Expectation)
import Length
import Point2d
import RegularPolygon2d
import Test exposing (Test, describe, test)



-------- Expectation tests


just : Maybe a -> Expectation
just result =
    case result of
        Just _ ->
            Expect.pass

        Nothing ->
            Expect.fail "Should return a value"


nothing : Maybe a -> Expectation
nothing result =
    case result of
        Just _ ->
            Expect.fail "Should return nothing"

        Nothing ->
            Expect.pass



-------- Tests


from : Test
from =
    describe "Regular polygon builder"
        [ test "should allow creation of triangle" <|
            \_ ->
                RegularPolygon2d.from
                    { sides = 3
                    , radius = Length.meters 3
                    , angle = Angle.degrees 0
                    , position = Point2d.origin
                    }
                    |> just
        , test "Should not allow creation of 2 sided polygon" <|
            \_ ->
                RegularPolygon2d.from
                    { sides = 2
                    , radius = Length.meters 3
                    , angle = Angle.degrees 0
                    , position = Point2d.origin
                    }
                    |> nothing
        ]
