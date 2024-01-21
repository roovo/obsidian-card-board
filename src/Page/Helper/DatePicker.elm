module Page.Helper.DatePicker exposing
    ( DatePicker
    , Msg
    , init
    , view
    )

import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes exposing (placeholder, type_, value)



-- MODEL


type DatePicker
    = DatePicker Model


type alias Model =
    { inputText : String
    , date : Maybe Date
    }


init : Maybe Date -> DatePicker
init date =
    DatePicker
        { inputText = toDateString date
        , date = date
        }



-- UPDATE


type Msg
    = NoOp



-- VIEW


view : DatePicker -> Html Msg
view (DatePicker model) =
    Html.input
        [ type_ "text"
        , placeholder "Due date"
        , value model.inputText
        ]
        []



-- PRIVATE


toDateString : Maybe Date -> String
toDateString =
    Maybe.withDefault "" << Maybe.map Date.toIsoString
