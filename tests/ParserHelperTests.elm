module ParserHelperTests exposing (suite)

import Expect exposing (Expectation)
import Parser exposing ((|=))
import ParserHelper exposing (anyLineParser, nonEmptyStringParser, wordParser)
import TaskItem
import Test exposing (..)


suite : Test
suite =
    concat
        [ nonEmptyStringParserTest
        , anyLineParserTest
        , wordParserTest
        ]


nonEmptyStringParserTest : Test
nonEmptyStringParserTest =
    describe "parsing non-empty strings"
        [ test "'foo' parses Ok" <|
            \() ->
                "foo"
                    |> Parser.run nonEmptyStringParser
                    |> Expect.equal (Ok "foo")
        , test "'foo bar' parses Ok" <|
            \() ->
                "foo bar"
                    |> Parser.run nonEmptyStringParser
                    |> Expect.equal (Ok "foo bar")
        , test "parses only the first line of a multiline string" <|
            \() ->
                """foo
bar
baz
"""
                    |> Parser.run nonEmptyStringParser
                    |> Expect.equal (Ok "foo")
        , test "fails with an empty string" <|
            \() ->
                ""
                    |> Parser.run nonEmptyStringParser
                    |> Result.withDefault "eek"
                    |> Expect.equal "eek"
        , test "fails with multiline string with empty first line" <|
            \() ->
                """
bar
baz
"""
                    |> Parser.run nonEmptyStringParser
                    |> Result.withDefault "eek"
                    |> Expect.equal "eek"
        ]


anyLineParserTest : Test
anyLineParserTest =
    describe "parsing any line"
        [ test "empty string fails to parse" <|
            \() ->
                ""
                    |> Parser.run anyLineParser
                    |> Result.withDefault "eek"
                    |> Expect.equal "eek"
        , test "'foo' parses" <|
            \() ->
                "foo"
                    |> Parser.run anyLineParser
                    |> Expect.equal (Ok "foo")
        , test "'foo<eol>' parses" <|
            \() ->
                "foo\n"
                    |> Parser.run anyLineParser
                    |> Expect.equal (Ok "foo")
        , test "'<eol>' parses" <|
            \() ->
                "\n"
                    |> Parser.run anyLineParser
                    |> Expect.equal (Ok "")
        , test "parses multiple lines" <|
            \() ->
                "foo\nbar\n"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= anyLineParser
                            |= anyLineParser
                        )
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "won't parse beyond the end of the input" <|
            \() ->
                "foo"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= anyLineParser
                            |= anyLineParser
                        )
                    |> Result.withDefault [ "eek" ]
                    |> Expect.equal [ "eek" ]
        ]


wordParserTest : Test
wordParserTest =
    describe "parsing a single word"
        [ test "'foo' parses Ok" <|
            \() ->
                "foo"
                    |> Parser.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "'foo<eol>' parses Ok" <|
            \() ->
                "foo\n"
                    |> Parser.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "'foo bar' parses Ok picking just 'foo'" <|
            \() ->
                "foo bar"
                    |> Parser.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "'foo<eol>bar' parses Ok picking just 'foo'" <|
            \() ->
                "foo\nbar"
                    |> Parser.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "parses multiple words" <|
            \() ->
                "foo bar"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= wordParser
                            |= wordParser
                        )
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "fails with an empty string" <|
            \() ->
                ""
                    |> Parser.run wordParser
                    |> Result.withDefault "eek"
                    |> Expect.equal "eek"
        , test "won't parse beyond the end of the line" <|
            \() ->
                "foo\nbar"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= wordParser
                            |= wordParser
                        )
                    |> Result.withDefault [ "eek" ]
                    |> Expect.equal [ "eek" ]
        , test "won't parse beyond the end of the input" <|
            \() ->
                "foo"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= wordParser
                            |= wordParser
                        )
                    |> Result.withDefault [ "eek" ]
                    |> Expect.equal [ "eek" ]
        ]
