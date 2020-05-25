module Main exposing (main)

import AspectRatio
import Browser
import Browser.Dom
import Color
import Geometry.Svg
import Html exposing (Html)
import Html.Attributes
import Pixels exposing (Pixels)
import Point2d
import Quantity
import RegularPolygon2d exposing (RegularPolygon2d)
import Size exposing (Size)
import Svg exposing (Svg)
import Task
import Transmutation exposing (Transmutation)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Paint(..))
import Visualize


type Msg
    = GotViewport Browser.Dom.Viewport
    | WindowResize ( Float, Float )


type alias Model =
    { view : Size Pixels
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { view = Size.size Quantity.zero Quantity.zero
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport { scene, viewport } ->
            ( { model
                | view =
                    Size.size
                        (Pixels.pixels viewport.width)
                        (Pixels.pixels viewport.height)
              }
            , Cmd.none
            )

        WindowResize ( width, height ) ->
            ( { model
                | view =
                    Size.size
                        (Pixels.pixels width)
                        (Pixels.pixels height)
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        aspectRatio =
            AspectRatio.fromSize model.view

        center =
            Point2d.xy (Quantity.float <| AspectRatio.x aspectRatio / 2) (Quantity.float <| AspectRatio.y aspectRatio / 2)

        radius =
            Quantity.float <| 0.4 * min (AspectRatio.x aspectRatio) (AspectRatio.y aspectRatio)

        startingPolygon =
            RegularPolygon2d.fromUnsafe
                { sides = 5
                , radius = radius
                , center = center
                , angle = Quantity.zero
                }

        startingPolygonSvg =
            startingPolygon
                |> RegularPolygon2d.asPolygon2d
                |> Geometry.Svg.polygon2d
                    [ TypedSvg.Attributes.stroke <| Paint Color.black
                    , TypedSvg.Attributes.InPx.strokeWidth 0.01
                    , TypedSvg.Attributes.noFill
                    ]

        svg =
            TypedSvg.svg
                [ TypedSvg.Attributes.viewBox 0 0 aspectRatio.x aspectRatio.y
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ]
                [ startingPolygonSvg
                , Visualize.transmutation <| generateTransmutation startingPolygon
                ]
    in
    Html.div [] [ svg ]


generateTransmutation : RegularPolygon2d units coordinates -> Transmutation units coordinates
generateTransmutation startingPolygon =
    Transmutation.midpointInsetAndFork startingPolygon
        |> Transmutation.withInternal Transmutation.midpointInsetAndFork
        |> Transmutation.withFork Transmutation.midpointInsetAndFork
