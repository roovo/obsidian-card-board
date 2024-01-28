module Worker exposing (main)

import Json.Decode as JD


main : Program JD.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { foo : String }


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    ( { foo = "bar" }, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
