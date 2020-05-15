module RegularPolygon2d exposing
    ( RegularPolygon2d
    , from, fromUnsafe
    , sides, radius, angle, position, exteriorAngle
    , scale, rotateRelativeToExteriorAngle, rotateHalfExteriorAngle
    )

{-| A 2d regular polygon module. Regular polygons are polygons with a certain number of vertices and sides where all
the sides are equal length and the internal angles are equivalent.


# Type

@docs RegularPolygon2d


# Build

@docs from, fromUnsafe


# Accessors

@docs sides, radius, angle, position, exteriorAngle


# Modifiers

@docs scale, rotateRelativeToExteriorAngle, rotateHalfExteriorAngle

@ docs scale

-}

import Angle exposing (Angle)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)



-------- Types


type RegularPolygon2d units coordinates
    = RegularPolygon2d
        { sides : Int
        , radius : Quantity Float units
        , angle : Angle
        , position : Point2d units coordinates
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
        , position = Point2d.origin
        }

-}
from :
    { sides : Int
    , radius : Quantity Float units
    , angle : Angle
    , position : Point2d units coordinates
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
    , position : Point2d units coordinates
    }
    -> RegularPolygon2d units coordinates
fromUnsafe =
    RegularPolygon2d



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


angle : RegularPolygon2d units coordinates -> Angle
angle polygon =
    case polygon of
        RegularPolygon2d records ->
            records.angle


position : RegularPolygon2d units coordinates -> Point2d units coordinates
position polygon =
    case polygon of
        RegularPolygon2d records ->
            records.position


{-| Get the exterior angle of a polygon. The exterior angle is the angle amount between two of the external vertices.
The sum of all the exterior angles should be 360 degrees, so the exterior angle is 360/n degrees where n is the number
of sides of the regular polygon.
-}
exteriorAngle : RegularPolygon2d units coordinates -> Angle
exteriorAngle polygon =
    Quantity.divideBy (toFloat <| sides polygon) (Angle.radians <| 2 * pi)



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
