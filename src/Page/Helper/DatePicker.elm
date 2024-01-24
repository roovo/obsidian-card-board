module Page.Helper.DatePicker exposing
    ( DatePicker
    , Msg
    , init
    , isValid
    , pickedDate
    , update
    , view
    )

import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, placeholder, tabindex, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, stopPropagationOn)
import Json.Decode as JD
import List.Extra as LE
import Time



-- MODEL


type DatePicker
    = DatePicker Model


type alias Model =
    { calendarStart : Date
    , firstDayOfWeek : Time.Weekday
    , inputText : String
    , mouseDownInPicker : Bool
    , showPicker : Bool
    , today : Date
    }


init : Time.Weekday -> Date -> Maybe Date -> DatePicker
init firstDayOfWeek today date =
    let
        calendarStart =
            Date.fromCalendarDate (Date.year startDate) (Date.month startDate) 1

        startDate =
            Maybe.withDefault today date
    in
    DatePicker
        { calendarStart = calendarStart
        , firstDayOfWeek = firstDayOfWeek
        , inputText = toDateString date
        , mouseDownInPicker = False
        , showPicker = False
        , today = today
        }



-- INFO


isValid : DatePicker -> Bool
isValid datePicker =
    not <| pickedDate datePicker == Nothing && hasInput datePicker


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
    | NextMonthClicked
    | PickerMouseDown
    | PickerMouseUp
    | PreviousMonthClicked
    | ThisMonthClicked


update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case msg of
        Blurred ->
            DatePicker { model | showPicker = model.mouseDownInPicker }

        DateClicked date ->
            DatePicker { model | inputText = Date.toIsoString date, showPicker = False }

        EnteredDate dateString ->
            DatePicker { model | inputText = dateString }

        Focussed ->
            DatePicker { model | showPicker = True, mouseDownInPicker = False }

        NextMonthClicked ->
            DatePicker { model | calendarStart = Date.add Date.Months 1 model.calendarStart }

        PickerMouseDown ->
            DatePicker { model | mouseDownInPicker = True }

        PickerMouseUp ->
            DatePicker { model | mouseDownInPicker = False }

        PreviousMonthClicked ->
            DatePicker { model | calendarStart = Date.add Date.Months -1 model.calendarStart }

        ThisMonthClicked ->
            let
                calendarStart =
                    Date.fromCalendarDate (Date.year model.today) (Date.month model.today) 1
            in
            DatePicker { model | calendarStart = calendarStart }



-- VIEW


view : DatePicker -> Html Msg
view ((DatePicker model) as datePicker) =
    let
        validityClass =
            if isValid datePicker then
                ""

            else
                "datepicker-error"
    in
    Html.div [ class "datepicker-container" ]
        [ Html.input
            [ type_ "text"
            , class "datepicker-input"
            , class validityClass
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
            [ Html.div [ class "datepicker-prev-container" ]
                [ Html.div
                    [ class "datepicker-prev"
                    , onClick PreviousMonthClicked
                    ]
                    []
                ]
            , Html.div [ class "datepicker-month-container" ]
                [ Html.span [ class "datepicker-month" ]
                    [ Html.text <| Date.format "MMMM" model.calendarStart ]
                , Html.span [ class "datepicker-year" ]
                    [ Html.text <| String.fromInt <| Date.year model.calendarStart ]
                ]
            , Html.div [ class "datepicker-next-container" ]
                [ Html.div
                    [ class "datepicker-next"
                    , onClick NextMonthClicked
                    ]
                    []
                ]
            ]
        , Html.div [ class "datepicker-this-month-jump-container" ]
            [ Html.span
                [ class "datepicker-this-month-jump"
                , onClick ThisMonthClicked
                ]
                [ Html.text "this month" ]
            ]
        , Html.table [ class "datepicker-table" ]
            [ Html.thead [ class "datepicker-weekdays" ]
                [ Html.tr []
                    ([ "Mo", "Tu", "We", "Th", "Fr", "Sa", "Su" ]
                        |> List.repeat 2
                        |> List.concat
                        |> List.drop (Date.weekdayToNumber model.firstDayOfWeek - 1)
                        |> List.take 7
                        |> List.map (\d -> Html.td [ class "datepicker-dow" ] [ Html.text d ])
                    )
                ]
            , Html.tbody [ class "datepicker-days" ]
                (model.calendarStart
                    |> dateGrid model.firstDayOfWeek
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


dateGrid : Time.Weekday -> Date -> List (List Date)
dateGrid firstDayOfWeek calendarStart =
    let
        firstDayAsInterval =
            weekdayToInterval firstDayOfWeek

        start =
            calendarStart
                |> Date.floor firstDayAsInterval

        end =
            Date.add Date.Months 1 calendarStart
                |> Date.ceiling firstDayAsInterval
    in
    Date.range Date.Day 1 start end
        |> LE.groupsOf 7


toDateString : Maybe Date -> String
toDateString =
    Maybe.withDefault "" << Maybe.map Date.toIsoString


weekdayToInterval : Time.Weekday -> Date.Interval
weekdayToInterval weekday =
    case weekday of
        Time.Mon ->
            Date.Monday

        Time.Tue ->
            Date.Tuesday

        Time.Wed ->
            Date.Wednesday

        Time.Thu ->
            Date.Thursday

        Time.Fri ->
            Date.Friday

        Time.Sat ->
            Date.Saturday

        Time.Sun ->
            Date.Sunday


hasInput : DatePicker -> Bool
hasInput (DatePicker model) =
    model.inputText
        |> String.trim
        |> String.isEmpty
        |> not
