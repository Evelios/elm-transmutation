module Test.RegularPolygon2d exposing (..)

import Angle
import Debug
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import Point2d
import Polygon2d exposing (Polygon2d)
import Quantity
import RegularPolygon2d exposing (RegularPolygon2d)
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


fuzzRegularPolygon2d : Fuzzer (RegularPolygon2d Meters coordinates)
fuzzRegularPolygon2d =
    Fuzz.map4
        (\sides radius angle center ->
            RegularPolygon2d.fromUnsafe
                { sides = sides
                , radius = radius
                , angle = angle
                , center = center
                }
        )
        (Fuzz.intRange 3 50)
        Fuzz.length
        Fuzz.angle
        Fuzz.point2d



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
                    , center = Point2d.origin
                    }
                    |> just
        , test "Should not allow creation of 2 sided polygon" <|
            \_ ->
                RegularPolygon2d.from
                    { sides = 2
                    , radius = Length.meters 3
                    , angle = Angle.degrees 0
                    , center = Point2d.origin
                    }
                    |> nothing
        ]


internalRadius : Test
internalRadius =
    describe "The internal radius"
        [ test "Should be 1/sqrt(2) for a square of radius 2" <|
            \_ ->
                let
                    actual =
                        RegularPolygon2d.fromUnsafe
                            { sides = 4
                            , radius = Length.meters 1
                            , angle = Angle.degrees 0
                            , center = Point2d.origin
                            }
                            |> RegularPolygon2d.internalRadius

                    expected =
                        Length.meters <| (1 / sqrt 2)
                in
                Expect.quantityWithin (Length.meters 0.001) expected actual
        ]


vertices : Test
vertices =
    describe "The vertices"
        [ Test.fuzz fuzzRegularPolygon2d
            "should be the center of the polygon"
            (\polygon ->
                let
                    maybeCentroid =
                        Polygon2d.singleLoop (RegularPolygon2d.vertices polygon)
                            |> Polygon2d.centroid
                in
                case maybeCentroid of
                    Just centroid ->
                        Expect.point2dWithin (Length.meters 0.001) (RegularPolygon2d.center polygon) centroid

                    Nothing ->
                        Expect.fail "Could not find centroid of regular polygon vertices"
            )
        , Test.fuzz fuzzRegularPolygon2d
            "should be the length of the radius away from the center"
            (\polygon ->
                let
                    lengths =
                        RegularPolygon2d.vertices polygon
                            |> List.map (Point2d.distanceFrom (RegularPolygon2d.center polygon))
                in
                lengths
                    |> List.all (Quantity.equalWithin (Length.meters 0.001) (RegularPolygon2d.radius polygon))
                    |> Expect.true
                        ("The length of a vertex is not equal to the radius of the polygon.\n"
                            ++ "Radius: "
                            ++ String.fromFloat (Length.inMeters (RegularPolygon2d.radius polygon))
                            ++ "\n"
                            ++ "Actual: "
                            ++ Debug.toString lengths
                        )
            )
        ]


midpoints : Test
midpoints =
    describe "The midpoints"
        [ Test.fuzz fuzzRegularPolygon2d
            "should be the center of the polygon"
            (\polygon ->
                let
                    maybeCentroid =
                        Polygon2d.singleLoop (RegularPolygon2d.midpoints polygon)
                            |> Polygon2d.centroid
                in
                case maybeCentroid of
                    Just centroid ->
                        Expect.point2dWithin (Length.meters 0.001) (RegularPolygon2d.center polygon) centroid

                    Nothing ->
                        Expect.fail "Could not find centroid of regular polygon vertices"
            )
        , Test.fuzz fuzzRegularPolygon2d
            "should be the length of the radius away from the center"
            (\polygon ->
                let
                    lengths =
                        RegularPolygon2d.midpoints polygon
                            |> List.map (Point2d.distanceFrom (RegularPolygon2d.center polygon))
                in
                lengths
                    |> List.all (Quantity.equalWithin (Length.meters 0.001) (RegularPolygon2d.internalRadius polygon))
                    |> Expect.true
                        ("The length of a midpoint is not equal to the internal radius of the polygon.\n"
                            ++ "Radius: "
                            ++ String.fromFloat (Length.inMeters (RegularPolygon2d.internalRadius polygon))
                            ++ "\n"
                            ++ "Actual: "
                            ++ Debug.toString lengths
                        )
            )
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
                            , center = Point2d.origin
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
                            , center = Point2d.origin
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
                            , center = Point2d.origin
                            }
                            |> RegularPolygon2d.scale 2

                    expected =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 6
                            , angle = Angle.degrees 0
                            , center = Point2d.origin
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
                            , center = Point2d.origin
                            }
                            |> RegularPolygon2d.rotateRelativeToExteriorAngle (1.0 / 3.0)

                    expected =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 40
                            , center = Point2d.origin
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
                            , center = Point2d.origin
                            }
                            |> RegularPolygon2d.rotateHalfExteriorAngle

                    expected =
                        RegularPolygon2d.fromUnsafe
                            { sides = 3
                            , radius = Length.meters 3
                            , angle = Angle.degrees 60
                            , center = Point2d.origin
                            }

                    toRadians polygon =
                        polygon
                            |> RegularPolygon2d.angle
                            |> Angle.inDegrees
                in
                Expect.within (Absolute 0.001) (toRadians actual) (toRadians expected)
        ]
