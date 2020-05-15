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


scale : Test
scale =
    describe "Scaling regular polygon"
        [ test "by 2 should return radius twice the original" <|
            \_ ->
                let
                    actual =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 0
                            , position = Point2d.origin
                            }
                            |> RegularPolygon2d.scale 2

                    expected =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 6
                            , angle = Angle.degrees 0
                            , position = Point2d.origin
                            }
                in
                Expect.equal actual expected
        ]


accessors : Test
accessors =
    let
        sides =
            3

        radius =
            Length.meters 3

        angle =
            Angle.degrees 0

        position =
            Point2d.origin

        polygon =
            RegularPolygon2d.fromUnsafe
                { sides = sides
                , radius = radius
                , angle = angle
                , position = position
                }
    in
    describe "Regular polygon accessors"
        [ test "should be able to get sides" <|
            \_ ->
                RegularPolygon2d.sides polygon
                    |> Expect.equal sides
        , test "should be able to get radius" <|
            \_ ->
                RegularPolygon2d.radius polygon
                    |> Expect.equal radius
        , test "should be able to get angle" <|
            \_ ->
                RegularPolygon2d.angle polygon
                    |> Expect.equal angle
        , test "should be able to get position" <|
            \_ ->
                RegularPolygon2d.position polygon
                    |> Expect.equal position
        ]
