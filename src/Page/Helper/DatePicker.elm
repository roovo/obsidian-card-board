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
import Html.Events exposing (onBlur, onFocus, onInput)
import Time



-- MODEL


type DatePicker
    = DatePicker Model


type alias Model =
    { calendarStart : Date
    , inputText : String
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
    | EnteredDate String
    | Focussed


update : Msg -> DatePicker -> DatePicker
update msg (DatePicker model) =
    case msg of
        Blurred ->
            DatePicker { model | showPicker = False }

        EnteredDate dateString ->
            DatePicker { model | inputText = dateString }

        Focussed ->
            DatePicker { model | showPicker = True }



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
    Html.div [ class "datepicker-picker" ]
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
                    |> displayDates
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

        isToday =
            Date.toRataDie date == Date.toRataDie model.today
    in
    Html.td
        [ classList
            [ ( "datepicker-day", True )
            , ( "datepicker-other-month", isOtherMonth )
            , ( "datepicker-today", isToday )
            ]
        ]
        [ Html.text <| String.fromInt <| Date.day date ]



-- PRIVATE


dateGrid : List Date -> List (List Date)
dateGrid dates =
    let
        go i xs racc acc =
            case xs of
                [] ->
                    List.reverse acc

                x :: xxs ->
                    if i == 6 then
                        go 0 xxs [] (List.reverse (x :: racc) :: acc)

                    else
                        go (i + 1) xxs (x :: racc) acc
    in
    go 0 dates [] []


displayDates : Date -> List Date
displayDates calendarStart =
    let
        start =
            calendarStart
                |> Date.floor Date.Monday

        end =
            Date.add Date.Months 1 calendarStart
                |> Date.ceiling Date.Monday
    in
    Date.range Date.Day 1 start end


toDateString : Maybe Date -> String
toDateString =
    Maybe.withDefault "" << Maybe.map Date.toIsoString
