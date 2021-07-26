module ParserHelperTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Parser exposing ((|.), (|=))
import ParserHelper exposing (anyLineParser, dateParser, nonEmptyStringParser, wordParser)
import TaskItem
import Test exposing (..)
import Time exposing (Month(..))


suite : Test
suite =
    concat
        [ nonEmptyStringParserTest
        , anyLineParserTest
        , wordParserTest
        , dateParserTest
        , checkWhitespaceFollowsTests
        ]


checkWhitespaceFollowsTests : Test
checkWhitespaceFollowsTests =
    describe "parser"
        [ test "parsers a valid token at end of string" <|
            \() ->
                "hi"
                    |> Parser.run (ParserHelper.checkWhitespaceFollows <| Parser.token "hi")
                    |> Expect.equal (Ok ())
        , test "succeeeds if followed by whitespace" <|
            \() ->
                "hi  "
                    |> Parser.run (ParserHelper.checkWhitespaceFollows <| Parser.token "hi")
                    |> Expect.equal (Ok ())
        , test "succeeeds if followed by a new line" <|
            \() ->
                "hi\nsomething else"
                    |> Parser.run (ParserHelper.checkWhitespaceFollows <| Parser.token "hi")
                    |> Expect.equal (Ok ())
        , test "does not consume following whitespace" <|
            \() ->
                "hi 12"
                    |> Parser.run
                        (Parser.succeed (\a b -> [ a, b ])
                            |= (ParserHelper.checkWhitespaceFollows <| Parser.token "hi")
                            |. Parser.token " "
                            |= Parser.token "12"
                        )
                    |> Expect.equal (Ok [ (), () ])
        , test "fails if followed by additional non-whitespaace" <|
            \() ->
                "hix"
                    |> Parser.run (ParserHelper.checkWhitespaceFollows <| Parser.token "hi")
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


dateParserTest : Test
dateParserTest =
    describe "parsing dates"
        [ test "parses valid date string" <|
            \() ->
                "2020-01-02"
                    |> Parser.run dateParser
                    |> Expect.equal (Ok <| Date.fromCalendarDate 2020 Jan 2)
        , test "fails with an invalie date string" <|
            \() ->
                "2020-41-02"
                    |> Parser.run dateParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
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
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "'foo' parses" <|
            -- this should parse but doesn't so I have to append \n on the end of input before parsing
            \() ->
                "foo"
                    |> Parser.run anyLineParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "'foo<eol>' parses" <|
            \() ->
                "foo\n"
                    |> Parser.run anyLineParser
                    |> Expect.equal (Ok ())
        , test "'<eol>' parses" <|
            \() ->
                "\n"
                    |> Parser.run anyLineParser
                    |> Expect.equal (Ok ())
        , test "parses multiple lines" <|
            \() ->
                "foo\nbar\n"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= anyLineParser
                            |= anyLineParser
                        )
                    |> Expect.equal (Ok [ (), () ])
        , test "won't parse beyond the end of the input" <|
            \() ->
                "foo"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= anyLineParser
                            |= anyLineParser
                        )
                    |> Result.toMaybe
                    |> Expect.equal Nothing
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
                            |. Parser.token " "
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
