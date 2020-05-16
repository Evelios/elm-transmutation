module Main exposing (main)

import AspectRatio
import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Pixels exposing (Pixels)
import Quantity
import Size exposing (Size)
import Svg exposing (Svg)
import Task
import TypedSvg
import TypedSvg.Attributes


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

        svg =
            TypedSvg.svg
                [ TypedSvg.Attributes.viewBox 0 0 aspectRatio.x aspectRatio.y
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                ]
                []
    in
    Html.div [] [ svg ]
