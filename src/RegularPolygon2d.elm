module RegularPolygon2d exposing
    ( RegularPolygon2d
    , from, fromUnsafe
    , sides, radius, internalRadius, angle, center, vertices, midpoints, exteriorAngle, edgeLength
    , asPolygon2d
    , scale, rotateRelativeToExteriorAngle, rotateHalfExteriorAngle
    )

{-| A 2d regular polygon module. Regular polygons are polygons with a certain number of vertices and sides where all
the sides are equal length and the internal angles are equivalent.


# Type

@docs RegularPolygon2d


# Build

@docs from, fromUnsafe


# Accessors

@docs sides, radius, internalRadius, angle, center, vertices, midpoints, exteriorAngle, edgeLength


# Conversions

@docs asPolygon2d


# Modifiers

@docs scale, rotateRelativeToExteriorAngle, rotateHalfExteriorAngle

@ docs scale

-}

import Angle exposing (Angle)
import List.Util
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity)
import Vector2d



-------- Types


type RegularPolygon2d units coordinates
    = RegularPolygon2d
        { sides : Int
        , radius : Quantity Float units
        , angle : Angle
        , center : Point2d units coordinates
        }



-------- Builders


{-| Create a regular polygon with a particular number of sides. This polygon is of a particular radius and angle located
somewhere in the 2d plane.

    import Length
    import Angle
    import Point2d

    -- Create a pentagon slightly rotated at the origin
    from
        { sides = 5
        , radius = Length.meters 3
        , angle = Angle.degrees 10
        , center = Point2d.origin
        }

-}
from :
    { sides : Int
    , radius : Quantity Float units
    , angle : Angle
    , center : Point2d units coordinates
    }
    -> Maybe (RegularPolygon2d units coordinates)
from properties =
    if properties.sides > 2 then
        Just <| fromUnsafe properties

    else
        Nothing


{-| Create a regular polygon without any checking. This should only be done if you are hard coding values into the
constructor.
-}
fromUnsafe :
    { sides : Int
    , radius : Quantity Float units
    , angle : Angle
    , center : Point2d units coordinates
    }
    -> RegularPolygon2d units coordinates
fromUnsafe properties =
    RegularPolygon2d { properties | radius = Quantity.abs properties.radius }



-------- Accessors


sides : RegularPolygon2d units coordinates -> Int
sides polygon =
    case polygon of
        RegularPolygon2d records ->
            records.sides


radius : RegularPolygon2d units coordinates -> Quantity Float units
radius polygon =
    case polygon of
        RegularPolygon2d records ->
            records.radius


internalRadius : RegularPolygon2d units coordinates -> Quantity Float units
internalRadius polygon =
    Quantity.multiplyBy (Angle.cos <| Quantity.half <| exteriorAngle polygon) (radius polygon)


angle : RegularPolygon2d units coordinates -> Angle
angle polygon =
    case polygon of
        RegularPolygon2d records ->
            records.angle


center : RegularPolygon2d units coordinates -> Point2d units coordinates
center polygon =
    case polygon of
        RegularPolygon2d records ->
            records.center


{-| -}
edgeLength : RegularPolygon2d units coordinates -> Quantity Float units
edgeLength polygon =
    Quantity.twice <| Quantity.multiplyBy (Angle.sin <| Quantity.half <| exteriorAngle polygon) (radius polygon)


{-| Helper function for the vertices and midpoints.
-}
pointsWithRadiusAndAngle : Quantity Float units -> Angle -> RegularPolygon2d units coordinates -> List (Point2d units coordinates)
pointsWithRadiusAndAngle theRadius theAngle polygon =
    let
        angles =
            List.Util.linspace (sides polygon) 0 (2 * pi - (Angle.inRadians <| exteriorAngle polygon))
                |> List.map Angle.radians

        initialPoint =
            Point2d.translateBy (Vector2d.rTheta theRadius theAngle) (center polygon)
    in
    angles
        |> List.map (\currentAngle -> Point2d.rotateAround (center polygon) currentAngle initialPoint)


{-| Get the vertices of the polygon. The first vertex is the point that is at the angle of the polygon. The vertices
go counter clockwise around the polygon.
-}
vertices : RegularPolygon2d units coordinates -> List (Point2d units coordinates)
vertices polygon =
    pointsWithRadiusAndAngle (radius polygon) (angle polygon) polygon


{-| Get the midpoints of the polygon vertices. The first vertex is the point that is at the angle of the polygon. The
midpoints go counter clockwise around the polygon.
-}
midpoints : RegularPolygon2d units coordinates -> List (Point2d units coordinates)
midpoints polygon =
    let
        startingAngle =
            angle polygon |> Quantity.plus (Quantity.half (exteriorAngle polygon))
    in
    pointsWithRadiusAndAngle (internalRadius polygon) startingAngle polygon


{-| Get the exterior angle of a polygon. The exterior angle is the angle amount between two of the external vertices.
The sum of all the exterior angles should be 360 degrees, so the exterior angle is 360/n degrees where n is the number
of sides of the regular polygon.
-}
exteriorAngle : RegularPolygon2d units coordinates -> Angle
exteriorAngle polygon =
    Quantity.divideBy (toFloat <| sides polygon) (Angle.radians <| 2 * pi)



-------- Conversions


asPolygon2d : RegularPolygon2d units coordinates -> Polygon2d units coordinates
asPolygon2d regularPolygon =
    Polygon2d.singleLoop <| vertices regularPolygon



-------- Modifiers


{-| Scale the regular polygon about the center point.
-}
scale : Float -> RegularPolygon2d units coordinates -> RegularPolygon2d units coordinates
scale amount polygon =
    case polygon of
        RegularPolygon2d records ->
            fromUnsafe { records | radius = Quantity.multiplyBy amount records.radius }


{-| Rotate the regular polygon about the center point.
-}
rotate : Angle -> RegularPolygon2d units coordinates -> RegularPolygon2d units coordinates
rotate amount polygon =
    case polygon of
        RegularPolygon2d records ->
            fromUnsafe { records | angle = Quantity.plus amount records.angle }


{-| Rotate the regular polygon relative to the exterior angle. The exterior angle is the angle of each individual wedge
of the polygon. The wedge is the arc between vertices. Rotation of 1.0 is rotating one of the exterior angles.
-}
rotateRelativeToExteriorAngle : Float -> RegularPolygon2d units coordinates -> RegularPolygon2d units coordinates
rotateRelativeToExteriorAngle amount polygon =
    rotate (Quantity.multiplyBy amount <| exteriorAngle polygon) polygon


{-| Rotate the polygon a half of the exterior angle.
-}
rotateHalfExteriorAngle : RegularPolygon2d units coordinates -> RegularPolygon2d units coordinates
rotateHalfExteriorAngle =
    rotateRelativeToExteriorAngle 0.5
