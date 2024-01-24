module Page.Helper.DatePicker exposing
    ( DatePicker
    , Msg
    , init
    , pickedDate
    , update
    , view
    )

import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, placeholder, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, stopPropagationOn)
import Json.Decode as JD
import List.Extra as LE
import Time



-- MODEL


type DatePicker
    = DatePicker Model


type alias Model =
    { calendarStart : Date
    , inputText : String
    , mouseDownInPicker : Bool
    , showPicker : Bool
    , today : Date
    }


init : Date -> Maybe Date -> DatePicker
init today date =
    let
        calendarStart =
            Date.fromCalendarDate (Date.year startDate) (Date.month startDate) 1

        startDate =
            Maybe.withDefault today date
    in
    DatePicker
        { calendarStart = calendarStart
        , inputText = toDateString date
        , mouseDownInPicker = False
        , showPicker = False
        , today = today
        }



-- INFO


pickedDate : DatePicker -> Maybe Date
pickedDate (DatePicker model) =
    model.inputText
        |> Date.fromIsoString
        |> Result.toMaybe



-- UPDATE


type Msg
    = Blurred
    | DateClicked Date
    | EnteredDate String
    | Focussed
    | PickerMouseDown
    | PickerMouseUp


update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case msg of
        Blurred ->
            DatePicker { model | showPicker = model.mouseDownInPicker }

        DateClicked date ->
            DatePicker { model | inputText = Date.toIsoString date }

        EnteredDate dateString ->
            DatePicker { model | inputText = dateString }

        Focussed ->
            DatePicker { model | showPicker = True, mouseDownInPicker = False }

        PickerMouseDown ->
            DatePicker { model | mouseDownInPicker = True }

        PickerMouseUp ->
            DatePicker { model | mouseDownInPicker = False }



-- VIEW


view : DatePicker -> Html Msg
view (DatePicker model) =
    Html.div [ class "datepicker-container" ]
        [ Html.input
            [ type_ "text"
            , placeholder "Due date"
            , value model.inputText
            , onInput EnteredDate
            , onFocus Focussed
            , onBlur Blurred
            ]
            []
        , if model.showPicker then
            pickerView model

          else
            Html.text ""
        ]


pickerView : Model -> Html Msg
pickerView model =
    Html.div
        [ class "datepicker-picker"
        , stopPropagationOn "mousedown" <| JD.succeed ( PickerMouseDown, True )
        , stopPropagationOn "mouseup" <| JD.succeed ( PickerMouseUp, True )
        ]
        [ Html.div [ class "datepicker-header" ]
            []
        , Html.table [ class "datepicker-table" ]
            [ Html.thead [ class "datepicker-weekdays" ]
                [ Html.tr []
                    ([ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ]
                        |> List.map (\d -> Html.td [ class "datepicker-dow" ] [ Html.text d ])
                    )
                ]
            , Html.tbody [ class "datepicker-days" ]
                (model.calendarStart
                    |> dateGrid
                    |> List.map
                        (\rowDays ->
                            Html.tr [ class "datepicker-row" ]
                                (List.map (dayView model) rowDays)
                        )
                )
            ]
        ]


dayView : Model -> Date -> Html Msg
dayView model date =
    let
        isOtherMonth =
            Date.month date /= Date.month model.calendarStart

        isPicked =
            Date.toIsoString date == model.inputText

        isToday =
            Date.toRataDie date == Date.toRataDie model.today
    in
    Html.td
        ([ classList
            [ ( "datepicker-day", True )
            , ( "datepicker-other-month", isOtherMonth )
            , ( "datepicker-today", isToday )
            , ( "datepicker-picked", isPicked )
            ]
         ]
            ++ [ onClick <| DateClicked date ]
        )
        [ Html.text <| String.fromInt <| Date.day date ]



-- PRIVATE


dateGrid : Date -> List (List Date)
dateGrid calendarStart =
    let
        start =
            calendarStart
                |> Date.floor Date.Monday

        end =
            Date.add Date.Months 1 calendarStart
                |> Date.ceiling Date.Monday
    in
    Date.range Date.Day 1 start end
        |> LE.groupsOf 7


toDateString : Maybe Date -> String
toDateString =
    Maybe.withDefault "" << Maybe.map Date.toIsoString
