module Geometry exposing
    ( Geometry(..)
    , boundingBox
    , scaleAbout
    )

{-|


# Types

@docs Geometry


# Accessors

@docs boundingBox


# Modifiers

@docs scaleAbout

-}

import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import RegularPolygon2d exposing (RegularPolygon2d)


{-| -}
type Geometry units coordinates
    = Circle (Circle2d units coordinates)
    | RegularPolygon (RegularPolygon2d units coordinates)
    | Line (LineSegment2d units coordinates)


boundingBox : Geometry units coordinates -> BoundingBox2d units coordinates
boundingBox geometry =
    case geometry of
        Circle circle ->
            Circle2d.boundingBox circle

        RegularPolygon polygon ->
            RegularPolygon2d.boundingBox polygon

        Line line ->
            LineSegment2d.boundingBox line


scaleAbout : Point2d units coordinates -> Float -> Geometry units coordinates -> Geometry units coordinates
scaleAbout point amount geometry =
    case geometry of
        Circle circle ->
            Circle2d.scaleAbout point amount circle
                |> Circle

        RegularPolygon polygon ->
            RegularPolygon2d.scaleAbout point amount polygon
                |> RegularPolygon

        Line line ->
            LineSegment2d.scaleAbout point amount line
                |> Line
