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
        [ toString
        ]


startOf2024 : Int
startOf2024 =
    1704067200000


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
