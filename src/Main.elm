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
import Quantity exposing (Unitless)
import Random
import RegularPolygon2d exposing (RegularPolygon2d)
import Size exposing (Size)
import Task
import Transmutation exposing (Transmutation)
import Transmutation.Generate
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (Paint(..))
import Visualize


type YDownCoordinates
    = YDownCoordinates


type Msg
    = GotViewport Browser.Dom.Viewport
    | WindowResize ( Float, Float )
    | NewTransmutation (Transmutation Unitless YDownCoordinates)


type alias Model =
    { view : Size Pixels
    , transmutation : Maybe (Transmutation Unitless YDownCoordinates)
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
      , transmutation = Nothing
      }
    , Cmd.batch
        [ Task.perform GotViewport Browser.Dom.getViewport
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport { scene, viewport } ->
            let
                newModel =
                    { model
                        | view =
                            Size.size
                                (Pixels.pixels viewport.width)
                                (Pixels.pixels viewport.height)
                    }
            in
            ( newModel
            , generateTransmutation newModel
            )

        WindowResize ( width, height ) ->
            let
                newModel =
                    { model
                        | view =
                            Size.size
                                (Pixels.pixels width)
                                (Pixels.pixels height)
                    }
            in
            ( newModel
            , Cmd.none
            )

        NewTransmutation transmutation ->
            ( { model | transmutation = Just transmutation }
            , Cmd.none
            )


generateTransmutation : Model -> Cmd Msg
generateTransmutation model =
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
    in
    Transmutation.Generate.transmutation
        { startingAlgorithm = Transmutation.midpointInsetAndFork
        , algorithms = [ Transmutation.cross ]
        , startingPolygon = startingPolygon
        , terminalCondition = \_ -> True
        }
        |> Random.generate NewTransmutation


view : Model -> Html Msg
view model =
    case model.transmutation of
        Just transmutation ->
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
                        , Visualize.transmutation transmutation
                        ]
            in
            Html.div [] [ svg ]

        Nothing ->
            Html.div [] []
