module ObsidianTasksDateTests exposing (suite)

import Date
import Expect
import ObsidianTasksDate
import Parser
import Test exposing (..)


suite : Test
suite =
    concat
        [ parser
        ]


parser : Test
parser =
    describe "parser"
        [ test "parsers a valid due date (📅)" <|
            \() ->
                "📅 2022-10-08"
                    |> Parser.run ObsidianTasksDate.parser
                    |> Expect.equal (Ok <| ObsidianTasksDate.Due (Date.fromRataDie 738436))
        , test "parsers a valid scheduled date (⏳)" <|
            \() ->
                "⏳ 2022-10-08"
                    |> Parser.run ObsidianTasksDate.parser
                    |> Expect.equal (Ok <| ObsidianTasksDate.Scheduled (Date.fromRataDie 738436))
        , test "parsers a valid completed date (✅)" <|
            \() ->
                "✅ 2022-10-08"
                    |> Parser.run ObsidianTasksDate.parser
                    |> Expect.equal (Ok <| ObsidianTasksDate.Completed (Date.fromRataDie 738436))
        , test "fails for the 'start' emoji" <|
            \() ->
                "🛫 2022-10-08"
                    |> Parser.run ObsidianTasksDate.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails if the date is not valid" <|
            \() ->
                "📅 2022-10-32"
                    |> Parser.run ObsidianTasksDate.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
