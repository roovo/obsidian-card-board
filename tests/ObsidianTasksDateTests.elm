module ObsidianTasksDateTests exposing (suite)

import Date
import DueDate
import Expect
import ObsidianTasksDate
import Parser
import Test exposing (..)
import Time


suite : Test
suite =
    concat
        [ completionTimeParser
        , dueDateParser
        ]


completionTimeParser : Test
completionTimeParser =
    describe "completionTimeParser"
        [ test "parsers a valid completion date (✅)" <|
            \() ->
                "✅ 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.completionTimeParser identity)
                    |> Expect.equal (Ok <| Time.millisToPosix 1665187200000)
        , test "fails if the completion date is not valid" <|
            \() ->
                "✅ 2022-10-32"
                    |> Parser.run (ObsidianTasksDate.completionTimeParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails for a valid due date (📅)" <|
            \() ->
                "📅 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.completionTimeParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails for a valid start date (🛫)" <|
            \() ->
                "🛫 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.completionTimeParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails for a valid scheduled date (⏳)" <|
            \() ->
                "⏳ 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.completionTimeParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


dueDateParser : Test
dueDateParser =
    describe "dueDateParser"
        [ test "parsers a valid due date (📅)" <|
            \() ->
                "📅 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.dueDateParser identity)
                    |> Expect.equal (Ok <| DueDate.SetToDate <| Date.fromRataDie 738436)
        , test "fails if the due date is not valid" <|
            \() ->
                "📅 2022-10-32"
                    |> Parser.run (ObsidianTasksDate.dueDateParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails for a valid completion date (✅)" <|
            \() ->
                "✅ 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.dueDateParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails for a valid start date (🛫)" <|
            \() ->
                "🛫 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.dueDateParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails for a valid scheduled date (⏳)" <|
            \() ->
                "⏳ 2022-10-08"
                    |> Parser.run (ObsidianTasksDate.dueDateParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
