module Page.Helper.DatePicker exposing
    ( DatePicker
    , Msg
    , init
    , update
    , view
    )

import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput)



-- MODEL


type DatePicker
    = DatePicker Model


type alias Model =
    { inputText : String
    }


init : Maybe Date -> DatePicker
init date =
    DatePicker
        { inputText = toDateString date
        }



-- UPDATE


type Msg
    = EnteredDate String


update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case msg of
        EnteredDate dateString ->
            DatePicker { model | inputText = dateString }



-- VIEW


view : DatePicker -> Html Msg
view (DatePicker model) =
    Html.input
        [ type_ "text"
        , placeholder "Due date"
        , value model.inputText
        , onInput EnteredDate
        ]
        []



-- PRIVATE


toDateString : Maybe Date -> String
toDateString =
    Maybe.withDefault "" << Maybe.map Date.toIsoString
