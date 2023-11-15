module Helpers.DateTimeHelpers exposing
    ( farFuture
    , future
    , now
    , offsetDateString
    , offsetNowWithZone
    , today
    , todayDate
    , tomorrow
    , yearStart
    , yesterday
    , zone
    )

import Date exposing (Date)
import Iso8601
import Time exposing (Posix)
import TimeWithZone exposing (TimeWithZone)



-- DATE STRINGS


yesterday : String
yesterday =
    "2020-06-19"


today : String
today =
    "2020-06-20"


tomorrow : String
tomorrow =
    "2020-06-21"


future : String
future =
    "2020-06-22"


farFuture : String
farFuture =
    "2020-06-23"



-- DATES


todayDate : Date
todayDate =
    TimeWithZone.toDate nowWithZone



-- TIME WITH ZONES


now : Posix
now =
    offsetNowWithZone 0
        |> .time


nowWithZone : TimeWithZone
nowWithZone =
    { time = now
    , zone = zone
    }


offsetDateString : Int -> String
offsetDateString offset =
    offsetNowWithZone offset
        |> .time
        |> Iso8601.fromTime
        |> String.left 10


offsetNowWithZone : Int -> TimeWithZone
offsetNowWithZone offset =
    let
        targetTime : Posix
        targetTime =
            today
                |> Iso8601.toTime
                |> Result.map Time.posixToMillis
                |> Result.map ((+) (offset * millisPerDay))
                |> Result.withDefault 0
                |> Time.millisToPosix
    in
    { time = targetTime
    , zone = zone
    }


yearStart : Posix
yearStart =
    -- 2020-01-01
    Time.millisToPosix 1577836800000


zone : Time.Zone
zone =
    Time.utc



-- PRIVATE


millisPerDay : Int
millisPerDay =
    1000 * 60 * 60 * 24
