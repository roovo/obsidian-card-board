module TimeWithZone exposing
    ( TimeWithZone
    , completionString
    , toDate
    , toString
    , updateTime
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import Date exposing (Date)
import GlobalSettings exposing (TaskCompletionSettings)
import Time exposing (Zone)
import Time.Extra as TE



-- TYPES


type alias TimeWithZone =
    { time : Time.Posix
    , zone : Zone
    }



-- INFO


completionString : DataviewTaskCompletion -> TaskCompletionSettings -> TimeWithZone -> String
completionString dataviewTaskCompletion taskCompletionSettings timeWithZone =
    let
        timeStamp : String
        timeStamp =
            toString taskCompletionSettings timeWithZone
    in
    case taskCompletionSettings.format of
        GlobalSettings.NoCompletion ->
            ""

        GlobalSettings.ObsidianCardBoard ->
            "@completed(" ++ timeStamp ++ ")"

        GlobalSettings.ObsidianDataview ->
            case dataviewTaskCompletion of
                DataviewTaskCompletion.NoCompletion ->
                    ""

                DataviewTaskCompletion.Emoji ->
                    "✅ " ++ String.left 10 timeStamp

                DataviewTaskCompletion.Text t ->
                    "[" ++ t ++ ":: " ++ String.left 10 timeStamp ++ "]"

        GlobalSettings.ObsidianTasks ->
            "✅ " ++ String.left 10 timeStamp


toDate : TimeWithZone -> Date
toDate timeWithZone =
    Date.fromPosix timeWithZone.zone timeWithZone.time


toString : { a | inLocalTime : Bool, showUtcOffset : Bool } -> TimeWithZone -> String
toString settings timeWithZone =
    let
        zoneToUse : Zone
        zoneToUse =
            if settings.inLocalTime then
                timeWithZone.zone

            else
                Time.utc
    in
    toPaddedString 4 (Time.toYear zoneToUse timeWithZone.time)
        ++ "-"
        -- MM
        ++ toPaddedString 2 (fromMonth (Time.toMonth zoneToUse timeWithZone.time))
        ++ "-"
        -- DD
        ++ toPaddedString 2 (Time.toDay zoneToUse timeWithZone.time)
        ++ "T"
        -- HH
        ++ toPaddedString 2 (Time.toHour zoneToUse timeWithZone.time)
        ++ ":"
        -- mm
        ++ toPaddedString 2 (Time.toMinute zoneToUse timeWithZone.time)
        ++ ":"
        -- ss
        ++ toPaddedString 2 (Time.toSecond zoneToUse timeWithZone.time)
        ++ offsetString settings timeWithZone



-- MODIFICATION


updateTime : Time.Posix -> TimeWithZone -> TimeWithZone
updateTime t timeWithZone =
    { timeWithZone | time = t }



-- PRIVATE


fromMonth : Time.Month -> Int
fromMonth month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


offsetString : { a | inLocalTime : Bool, showUtcOffset : Bool } -> TimeWithZone -> String
offsetString settings timeWithZone =
    case ( settings.inLocalTime, settings.showUtcOffset ) of
        ( False, False ) ->
            ""

        ( False, True ) ->
            "Z"

        ( True, False ) ->
            ""

        ( True, True ) ->
            let
                offset : Int
                offset =
                    TE.toOffset timeWithZone.zone timeWithZone.time
            in
            offsetSign offset ++ offsetValue offset


offsetSign : Int -> String
offsetSign offset =
    if offset >= 0 then
        "+"

    else
        "-"


offsetValue : Int -> String
offsetValue offset =
    let
        hours : Int
        hours =
            abs offset // 60
    in
    toPaddedString 2 hours
        ++ ":"
        ++ toPaddedString 2 (abs offset - (hours * 60))


toPaddedString : Int -> Int -> String
toPaddedString digits time_ =
    String.padLeft digits '0' (String.fromInt time_)
