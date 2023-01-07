module TimeWithZone exposing
    ( TimeWithZone
    , time
    , toDate
    )

import Date exposing (Date)
import Time


type alias TimeWithZone =
    { time : Time.Posix
    , zone : Time.Zone
    }


time : Time.Posix -> TimeWithZone -> TimeWithZone
time t timeWithZone =
    { timeWithZone | time = t }


toDate : TimeWithZone -> Date
toDate timeWithZone =
    Date.fromPosix timeWithZone.zone timeWithZone.time
