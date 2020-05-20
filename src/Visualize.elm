module Visualize exposing (..)

import Color
import Geometry exposing (Geometry(..))
import Geometry.Svg
import RegularPolygon2d
import Svg exposing (Attribute, Svg)
import Transmutation exposing (Transmutation)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Paint(..))


transmutation : Transmutation units coordinates -> Svg msg
transmutation trans =
    let
        attributes =
            [ TypedSvg.Attributes.InPx.strokeWidth 0.01
            , TypedSvg.Attributes.stroke <| Paint Color.black
            ]
    in
    Transmutation.geometry trans
        |> List.map (geometryToSvg attributes)
        |> Svg.svg []


geometryToSvg : List (Attribute msg) -> Geometry units coordinates -> Svg msg
geometryToSvg attributes geometry =
    case geometry of
        Circle circle ->
            Geometry.Svg.circle2d attributes circle

        RegularPolygon regularPolygon ->
            Geometry.Svg.polygon2d attributes (RegularPolygon2d.asPolygon2d regularPolygon)

        Line line ->
            Geometry.Svg.lineSegment2d attributes line
