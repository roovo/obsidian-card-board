module Helpers.DateTimeHelpers exposing
    ( farFuture
    , future
    , now
    , nowWithZone
    , today
    , tomorrow
    , yesterday
    , zone
    )

import Iso8601
import Time
import TimeWithZone exposing (TimeWithZone)


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


nowWithZone : TimeWithZone
nowWithZone =
    { now = now
    , zone = zone
    }


now : Time.Posix
now =
    today
        |> Iso8601.toTime
        |> Result.map Time.posixToMillis
        |> Result.withDefault 0
        |> Time.millisToPosix


zone : Time.Zone
zone =
    Time.utc
