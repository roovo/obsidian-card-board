module TimeWithZoneTests exposing (suite)

import DataviewTaskCompletion
import Expect
import Fuzz
import GlobalSettings
import Test exposing (..)
import Time
import TimeWithZone


suite : Test
suite =
    concat
        [ completionString
        , toString
        ]


startOf2024 : Int
startOf2024 =
    1704067200000


completionString : Test
completionString =
    describe "completionString"
        [ fuzz3 (Fuzz.intRange -1440 1440) Fuzz.bool Fuzz.bool "returns an empty string if the format is NoCompletion" <|
            \zone inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone zone [] }
                    |> TimeWithZone.completionString DataviewTaskCompletion.NoCompletion
                        { format = GlobalSettings.NoCompletion
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal ""
        , test "returns a utc completion string with no UTC offset if the format is ObsidianCardBoard" <|
            \() ->
                { time = Time.millisToPosix 0, zone = Time.customZone 0 [] }
                    |> TimeWithZone.completionString DataviewTaskCompletion.NoCompletion
                        { format = GlobalSettings.ObsidianCardBoard
                        , inLocalTime = False
                        , showUtcOffset = False
                        }
                    |> Expect.equal "@completed(1970-01-01T00:00:00)"
        , test "returns a local completion string with an UTC offset if the format is ObsidianCardBoard" <|
            \() ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> TimeWithZone.completionString DataviewTaskCompletion.NoCompletion
                        { format = GlobalSettings.ObsidianCardBoard
                        , inLocalTime = True
                        , showUtcOffset = True
                        }
                    |> Expect.equal "@completed(1970-01-01T05:00:00+05:00)"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is ObsidianTasks" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> TimeWithZone.completionString DataviewTaskCompletion.NoCompletion
                        { format = GlobalSettings.ObsidianTasks
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "✅ 1970-01-01"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is the default for ObsidianDataview" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> TimeWithZone.completionString DataviewTaskCompletion.default
                        { format = GlobalSettings.ObsidianDataview
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "[completion:: 1970-01-01]"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is a specified ObsidianDataview" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> TimeWithZone.completionString (DataviewTaskCompletion.Text "done")
                        { format = GlobalSettings.ObsidianDataview
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "[done:: 1970-01-01]"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is ObsidianDataview.Emoji" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> TimeWithZone.completionString DataviewTaskCompletion.Emoji
                        { format = GlobalSettings.ObsidianDataview
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "✅ 1970-01-01"
        ]


toString : Test
toString =
    describe "toString"
        [ describe "as utc string"
            [ describe "with no offset appended"
                [ test "returns the date and time string when the zone offset is 0" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 0 []
                        }
                            |> TimeWithZone.toString { inLocalTime = False, showUtcOffset = False }
                            |> Expect.equal "2024-01-01T00:00:00"
                , test "returns the date and time string when the zone offset is +60" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 60 []
                        }
                            |> TimeWithZone.toString { inLocalTime = False, showUtcOffset = False }
                            |> Expect.equal "2024-01-01T00:00:00"
                , test "returns the date and time string when the zone offset is -300" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone -300 []
                        }
                            |> TimeWithZone.toString { inLocalTime = False, showUtcOffset = False }
                            |> Expect.equal "2024-01-01T00:00:00"
                ]
            , describe "with the offset appended"
                [ test "appends a 'Z' on the end when the zone offset is 0" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 0 []
                        }
                            |> TimeWithZone.toString { inLocalTime = False, showUtcOffset = True }
                            |> Expect.equal "2024-01-01T00:00:00Z"
                , test "appends a 'Z' on the end when the zone offset is +60" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 60 []
                        }
                            |> TimeWithZone.toString { inLocalTime = False, showUtcOffset = True }
                            |> Expect.equal "2024-01-01T00:00:00Z"
                , test "appends a 'Z' on the end when the zone offset is e-300" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone -300 []
                        }
                            |> TimeWithZone.toString { inLocalTime = False, showUtcOffset = True }
                            |> Expect.equal "2024-01-01T00:00:00Z"
                ]
            ]
        , describe "as local time string"
            [ describe "with no offset appended"
                [ test "returns the date and time string when the zone offset is 0" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 0 []
                        }
                            |> TimeWithZone.toString { inLocalTime = True, showUtcOffset = False }
                            |> Expect.equal "2024-01-01T00:00:00"
                , test "returns the date and time string when the zone offset is +60" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 60 []
                        }
                            |> TimeWithZone.toString { inLocalTime = True, showUtcOffset = False }
                            |> Expect.equal "2024-01-01T01:00:00"
                , test "returns the date and time string when the zone offset is -300" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone -300 []
                        }
                            |> TimeWithZone.toString { inLocalTime = True, showUtcOffset = False }
                            |> Expect.equal "2023-12-31T19:00:00"
                ]
            , describe "with the offset appended"
                [ test "appends a '+00:00' on the end when the zone offset is 0" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 0 []
                        }
                            |> TimeWithZone.toString { inLocalTime = True, showUtcOffset = True }
                            |> Expect.equal "2024-01-01T00:00:00+00:00"
                , test "appends a '+01:00' on the end when the zone offset is +60" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone 60 []
                        }
                            |> TimeWithZone.toString { inLocalTime = True, showUtcOffset = True }
                            |> Expect.equal "2024-01-01T01:00:00+01:00"
                , test "appends a '-05:00' on the end when the zone offset is -300" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone -300 []
                        }
                            |> TimeWithZone.toString { inLocalTime = True, showUtcOffset = True }
                            |> Expect.equal "2023-12-31T19:00:00-05:00"
                , test "appends a '-05:30' on the end when the zone offset is -330" <|
                    \() ->
                        { time = Time.millisToPosix startOf2024
                        , zone = Time.customZone -330 []
                        }
                            |> TimeWithZone.toString { inLocalTime = True, showUtcOffset = True }
                            |> Expect.equal "2023-12-31T18:30:00-05:30"
                ]
            ]
        ]
