module Geometry exposing (Geometry(..))

{-| -}

import Circle2d exposing (Circle2d)
import LineSegment2d exposing (LineSegment2d)
import RegularPolygon2d exposing (RegularPolygon2d)


{-| -}
type Geometry units coordinates
    = Circle (Circle2d units coordinates)
    | RegularPolygon (RegularPolygon2d units coordinates)
    | Line (LineSegment2d units coordinates)
