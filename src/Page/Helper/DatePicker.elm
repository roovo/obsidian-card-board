module Page.Helper.DatePicker exposing
    ( DatePicker
    , init
    )

import Date exposing (Date)



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



-- PRIVATE


toDateString : Maybe Date -> String
toDateString =
    Maybe.withDefault "" << Maybe.map Date.toIsoString
