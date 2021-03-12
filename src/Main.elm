module Main exposing (main)

import Browser
import Html exposing (Html)


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \s -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    String


init : String -> ( Model, Cmd Msg )
init displayText =
    ( displayText
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.text model
