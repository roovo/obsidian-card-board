module TaskPaperTagTests exposing (suite)

import Date
import DueDate
import Expect
import Parser
import TaskPaperTag
import Test exposing (..)
import Time


suite : Test
suite =
    concat
        [ autocompleteTagParser
        , completedTagParser
        , dueTagParser
        , parser
        ]


autocompleteTagParser : Test
autocompleteTagParser =
    describe "autocompleteTagParser"
        [ test "parsers true value" <|
            \() ->
                "@autocomplete(true)"
                    |> Parser.run (TaskPaperTag.autocompleteTagParser identity)
                    |> Expect.equal (Ok True)
        , test "parsers false value" <|
            \() ->
                "@autocomplete(false)"
                    |> Parser.run (TaskPaperTag.autocompleteTagParser identity)
                    |> Expect.equal (Ok False)
        , test "fails with the value xXx" <|
            \() ->
                "@autocomplete(xXx)"
                    |> Parser.run (TaskPaperTag.autocompleteTagParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


completedTagParser : Test
completedTagParser =
    describe "completedTagParser"
        [ test "parsers a valid dateTime" <|
            \() ->
                "@completed(2023-01-01T09:00:00)"
                    |> Parser.run (TaskPaperTag.completedTagParser identity)
                    |> Result.map Time.posixToMillis
                    |> Expect.equal (Ok 1672563600000)
        , test "parses just a date" <|
            \() ->
                "@completed(2023-01-01)"
                    |> Parser.run (TaskPaperTag.completedTagParser identity)
                    |> Result.map Time.posixToMillis
                    |> Expect.equal (Ok 1672531200000)
        , test "parses if it is a dateTime without seconds" <|
            \() ->
                "@completed(2023-01-01T00:00)"
                    |> Parser.run (TaskPaperTag.completedTagParser identity)
                    |> Result.map Time.posixToMillis
                    |> Expect.equal (Ok 1672531200000)
        , test "fails if it is not a valid dateTime" <|
            \() ->
                "@completed(2023-01-01T00:00:0)"
                    |> Parser.run (TaskPaperTag.completedTagParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


dueTagParser : Test
dueTagParser =
    describe "dueTagParser"
        [ test "parsers a valid date" <|
            \() ->
                "@due(2023-01-01)"
                    |> Parser.run (TaskPaperTag.dueTagParser identity)
                    |> Expect.equal (Ok <| DueDate.SetToDate <| Date.fromCalendarDate 2023 Time.Jan 1)
        , test "parsers @due(none) as DueDate.SetToNone" <|
            \() ->
                "@due(none)"
                    |> Parser.run (TaskPaperTag.dueTagParser identity)
                    |> Expect.equal (Ok DueDate.SetToNone)
        , test "fails to parse an invalid date" <|
            \() ->
                "@due(2023-02-30)"
                    |> Parser.run (TaskPaperTag.dueTagParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails to parse a dateTime" <|
            \() ->
                "@due(2023-01-01T00:00:00)"
                    |> Parser.run (TaskPaperTag.dueTagParser identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


parser : Test
parser =
    describe "parser"
        [ test "parsers a valid tag with value" <|
            \() ->
                "@foo(33)"
                    |> Parser.run (TaskPaperTag.parser "foo" Parser.int identity)
                    |> Expect.equal (Ok 33)
        , test "fails if the key doesn't match" <|
            \() ->
                "@foox(33)"
                    |> Parser.run (TaskPaperTag.parser "foo" Parser.int identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails if the value is not parseable" <|
            \() ->
                "@foo(aa)"
                    |> Parser.run (TaskPaperTag.parser "foo" Parser.int identity)
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
