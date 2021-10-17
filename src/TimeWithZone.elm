module TimeWithZone exposing
    ( TimeWithZone
    , now
    , toDate
    )

import Date exposing (Date)
import Time


type alias TimeWithZone =
    { now : Time.Posix
    , zone : Time.Zone
    }


now : Time.Posix -> TimeWithZone -> TimeWithZone
now time timeWithZone =
    { timeWithZone | now = time }


toDate : TimeWithZone -> Date
toDate timeWithZone =
    Date.fromPosix timeWithZone.zone timeWithZone.now
