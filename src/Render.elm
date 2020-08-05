module Render exposing (..)

import Circle2d
import Collage exposing (Collage, Shape)
import Collage.Layout
import Collage.Render
import Color
import Geometry exposing (Geometry(..))
import Html exposing (Html)
import LineSegment2d
import Point2d
import Quantity exposing (Unitless)
import RegularPolygon2d
import Svg.Attributes
import Transmutation exposing (Transmutation)


transmutation : Transmutation Unitless coordinates -> Html msg
transmutation trans =
    let
        viewBox w h =
            List.map String.fromFloat [ -(w / 2), -(h / 2), w, h ]
                |> String.join " "
                |> Svg.Attributes.viewBox

        collageViewBox collage =
            viewBox (Collage.Layout.width collage) (Collage.Layout.height collage)
    in
    trans
        |> Transmutation.getGeometry
        |> List.map geometryToCollage
        |> Collage.Layout.stack
        |> (\collage ->
                Collage.Render.svgExplicit
                    [ collageViewBox collage ]
                    collage
           )


geometryToCollage : Geometry Unitless coordinates -> Collage msg
geometryToCollage geometry =
    let
        thin =
            0.005

        lineStyle =
            Collage.solid thin <| Collage.uniform Color.black

        shapeStyle =
            Collage.outlined lineStyle
    in
    case geometry of
        Circle circle ->
            Collage.circle (Quantity.toFloat <| Circle2d.radius circle)
                |> shapeStyle

        RegularPolygon regularPolygon ->
            Collage.ngon
                (RegularPolygon2d.sides regularPolygon)
                (Quantity.toFloat <| RegularPolygon2d.radius regularPolygon)
                |> shapeStyle

        Line line ->
            Collage.segment
                (Point2d.toTuple Quantity.toFloat <| LineSegment2d.startPoint line)
                (Point2d.toTuple Quantity.toFloat <| LineSegment2d.endPoint line)
                |> Collage.traced lineStyle
