module Test.RegularPolygon2d exposing (..)

import Angle
import Expect exposing (Expectation, FloatingPointTolerance(..))
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


basicAccessors : Test
basicAccessors =
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


exteriorAngle : Test
exteriorAngle =
    describe "The exterior angle"
        [ test "should be 120 degrees for a triangle" <|
            \_ ->
                let
                    actual =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 0
                            , position = Point2d.origin
                            }
                            |> RegularPolygon2d.exteriorAngle
                in
                actual |> Expect.equal (Angle.degrees 120)
        , test "should be 60 degrees for a hexagon" <|
            \_ ->
                let
                    actual =
                        RegularPolygon2d.fromUnsafe
                            { sides = 6
                            , radius = Length.meters 3
                            , angle = Angle.degrees 0
                            , position = Point2d.origin
                            }
                            |> RegularPolygon2d.exteriorAngle
                in
                actual |> Expect.equal (Angle.degrees 60)
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


rotateRelativeToExteriorAngle : Test
rotateRelativeToExteriorAngle =
    describe "Rotating relative to the exterior angle"
        [ test "should rotate 1/3 or 40 degrees for a triangle" <|
            \_ ->
                let
                    actual =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 0
                            , position = Point2d.origin
                            }
                            |> RegularPolygon2d.rotateRelativeToExteriorAngle (1.0 / 3.0)

                    expected =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 40
                            , position = Point2d.origin
                            }

                    toRadians polygon =
                        polygon
                            |> RegularPolygon2d.angle
                            |> Angle.inDegrees
                in
                Expect.within (Absolute 0.001) (toRadians actual) (toRadians expected)
        ]


rotateHalfExteriorAngle : Test
rotateHalfExteriorAngle =
    describe "Rotating half an exterior angle"
        [ test "should rotate 60 degrees for a triangle" <|
            \_ ->
                let
                    actual =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 0
                            , position = Point2d.origin
                            }
                            |> RegularPolygon2d.rotateHalfExteriorAngle

                    expected =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 60
                            , position = Point2d.origin
                            }

                    toRadians polygon =
                        polygon
                            |> RegularPolygon2d.angle
                            |> Angle.inDegrees
                in
                Expect.within (Absolute 0.001) (toRadians actual) (toRadians expected)
        ]
