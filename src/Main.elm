module Main exposing (main)

import Browser
import Browser.Dom
import Html exposing (Html)
import Pixels exposing (Pixels)
import Quantity exposing (Unitless)
import Random
import Render
import Size exposing (Size)
import Task
import Transmutation exposing (Transmutation)
import Transmutation.Generate as Generate


type Cartesian
    = Cartesian


type Msg
    = GotViewport Browser.Dom.Viewport
    | WindowResize ( Float, Float )
    | GenerateTransmutation
    | NewTransmutation (Transmutation Unitless Cartesian)


type alias Model =
    { view : Size Pixels
    , transmutation : Maybe (Transmutation Unitless Cartesian)
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
            newModel
                |> update GenerateTransmutation

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

        GenerateTransmutation ->
            ( model
            , Random.generate NewTransmutation (Generate.transmutation Generate.transmutationDefault)
            )

        NewTransmutation transmutation ->
            ( { model | transmutation = Just transmutation }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.transmutation of
        Just transmutation ->
            Render.transmutation transmutation

        Nothing ->
            Html.div [] []
