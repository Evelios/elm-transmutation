module RegularPolygon2d exposing
    ( RegularPolygon2d
    , from
    )

{-| A 2d regular polygon module. Regular polygons are polygons with a certain number of vertices and sides where all
the sides are equal length and the internal angles are equivalent.


# Type

@docs RegularPolygon2d


# Build

@docs from

-}

import Angle exposing (Angle)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)


type RegularPolygon2d units coordinates
    = RegularPolygon2d
        { sides : Int
        , radius : Quantity Float units
        , angle : Angle
        , position : Point2d units coordinates
        }


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
        Just <| RegularPolygon2d properties

    else
        Nothing
