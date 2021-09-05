module ParserHelperTests exposing (suite)

import Date
import Expect
import Parser as P exposing ((|.), (|=))
import ParserHelper exposing (anyLineParser, booleanParser, dateParser, nonEmptyStringParser, timeParser, wordParser)
import Test exposing (..)
import Time exposing (Month(..))


suite : Test
suite =
    concat
        [ anyLineParserTest
        , booleanParserTest
        , checkWhitespaceFollowsTests
        , dateParserTests
        , indentParserTests
        , nonEmptyStringParserTest
        , timeParserTests
        , wordParserTest
        ]


anyLineParserTest : Test
anyLineParserTest =
    describe "parsing any line"
        [ test "empty string fails to parse" <|
            \() ->
                ""
                    |> P.run anyLineParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "'foo' parses" <|
            -- this should parse but doesn't so I have to append \n on the end of input before parsing
            \() ->
                "foo"
                    |> P.run anyLineParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "'foo<eol>' parses" <|
            \() ->
                "foo\n"
                    |> P.run anyLineParser
                    |> Expect.equal (Ok "foo")
        , test "'<eol>' parses" <|
            \() ->
                "\n"
                    |> P.run anyLineParser
                    |> Expect.equal (Ok "")
        , test "parses multiple lines" <|
            \() ->
                "foo\nbar\n"
                    |> P.run
                        (P.succeed Tuple.pair
                            |= anyLineParser
                            |= anyLineParser
                        )
                    |> Expect.equal (Ok ( "foo", "bar" ))
        , test "won't parse beyond the end of the input" <|
            \() ->
                "foo\n"
                    |> P.run
                        (P.succeed Tuple.pair
                            |= anyLineParser
                            |= anyLineParser
                        )
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


booleanParserTest : Test
booleanParserTest =
    describe "parsing a boolean"
        [ test "true parses to True" <|
            \() ->
                "true"
                    |> P.run booleanParser
                    |> Expect.equal (Ok True)
        , test "false parses to True" <|
            \() ->
                "false"
                    |> P.run booleanParser
                    |> Expect.equal (Ok False)
        , test "truey fails to parse" <|
            \() ->
                "truey"
                    |> P.run booleanParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "empty string fails to parse" <|
            \() ->
                ""
                    |> P.run booleanParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


checkWhitespaceFollowsTests : Test
checkWhitespaceFollowsTests =
    describe "parser"
        [ test "parsers a valid token at end of string" <|
            \() ->
                "hi"
                    |> P.run (ParserHelper.checkWhitespaceFollows <| P.token "hi")
                    |> Expect.equal (Ok ())
        , test "succeeeds if followed by whitespace" <|
            \() ->
                "hi  "
                    |> P.run (ParserHelper.checkWhitespaceFollows <| P.token "hi")
                    |> Expect.equal (Ok ())
        , test "succeeeds if followed by a new line" <|
            \() ->
                "hi\nsomething else"
                    |> P.run (ParserHelper.checkWhitespaceFollows <| P.token "hi")
                    |> Expect.equal (Ok ())
        , test "does not consume following whitespace" <|
            \() ->
                "hi 12"
                    |> P.run
                        (P.succeed (\a b -> [ a, b ])
                            |= (ParserHelper.checkWhitespaceFollows <| P.token "hi")
                            |. P.token " "
                            |= P.token "12"
                        )
                    |> Expect.equal (Ok [ (), () ])
        , test "fails if followed by additional non-whitespaace" <|
            \() ->
                "hix"
                    |> P.run (ParserHelper.checkWhitespaceFollows <| P.token "hi")
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


dateParserTests : Test
dateParserTests =
    describe "parsing dates"
        [ test "parses valid date string" <|
            \() ->
                "2020-01-02"
                    |> P.run dateParser
                    |> Expect.equal (Ok <| Date.fromCalendarDate 2020 Jan 2)
        , test "fails with an invalie date string" <|
            \() ->
                "2020-41-02"
                    |> P.run dateParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


indentParserTests : Test
indentParserTests =
    describe "indent parser"
        [ test "parses an indented list of integers" <|
            \() ->
                """ 1
 2
 3
"""
                    |> P.run (ParserHelper.indentParser P.int)
                    |> Expect.equal (Ok [ 1, 2, 3 ])
        , test "parses an indented list of integers with a tab as the indent character" <|
            \() ->
                "\t1\n\t2\n\t3"
                    |> P.run (ParserHelper.indentParser P.int)
                    |> Expect.equal (Ok [ 1, 2, 3 ])
        , test "flattens all levels of indent" <|
            \() ->
                """ 1
  2
   3
"""
                    |> P.run (ParserHelper.indentParser P.int)
                    |> Expect.equal (Ok [ 1, 2, 3 ])
        , test "works with decreasing indent" <|
            \() ->
                """   1
  2
 3
4
"""
                    |> P.run (ParserHelper.indentParser P.int)
                    |> Expect.equal (Ok [ 1, 2, 3 ])
        , test "stops when there is no longer an indent" <|
            \() ->
                """ 1
  2
   3
4
"""
                    |> P.run (ParserHelper.indentParser P.int)
                    |> Expect.equal (Ok [ 1, 2, 3 ])
        , test "returns an empty list if there is no indent" <|
            \() ->
                """1
2
3
"""
                    |> P.run (ParserHelper.indentParser P.int)
                    |> Expect.equal (Ok [])
        ]


nonEmptyStringParserTest : Test
nonEmptyStringParserTest =
    describe "parsing non-empty strings"
        [ test "'foo' parses Ok" <|
            \() ->
                "foo"
                    |> P.run nonEmptyStringParser
                    |> Expect.equal (Ok "foo")
        , test "'foo bar' parses Ok" <|
            \() ->
                "foo bar"
                    |> P.run nonEmptyStringParser
                    |> Expect.equal (Ok "foo bar")
        , test "parses only the first line of a multiline string" <|
            \() ->
                """foo
bar
baz
"""
                    |> P.run nonEmptyStringParser
                    |> Expect.equal (Ok "foo")
        , test "fails with an empty string" <|
            \() ->
                ""
                    |> P.run nonEmptyStringParser
                    |> Result.withDefault "eek"
                    |> Expect.equal "eek"
        , test "fails with multiline string with empty first line" <|
            \() ->
                """
bar
baz
"""
                    |> P.run nonEmptyStringParser
                    |> Result.withDefault "eek"
                    |> Expect.equal "eek"
        ]


timeParserTests : Test
timeParserTests =
    describe "parsing times"
        [ test "parses valid time string containing only a date" <|
            \() ->
                "2020-01-02"
                    |> P.run timeParser
                    |> Expect.equal (Ok <| Time.millisToPosix 1577923200000)
        , test "parses valid time string with hh:mm:ss included" <|
            \() ->
                "2020-01-02T01:30:23"
                    |> P.run timeParser
                    |> Expect.equal (Ok <| Time.millisToPosix 1577928623000)
        , test "fails with an invalid time string" <|
            \() ->
                "2020-41-02"
                    |> P.run timeParser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


wordParserTest : Test
wordParserTest =
    describe "parsing a single word"
        [ test "'foo' parses Ok" <|
            \() ->
                "foo"
                    |> P.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "'foo<eol>' parses Ok" <|
            \() ->
                "foo\n"
                    |> P.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "'foo bar' parses Ok picking just 'foo'" <|
            \() ->
                "foo bar"
                    |> P.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "'foo<eol>bar' parses Ok picking just 'foo'" <|
            \() ->
                "foo\nbar"
                    |> P.run wordParser
                    |> Expect.equal (Ok "foo")
        , test "parses multiple words" <|
            \() ->
                "foo bar"
                    |> P.run
                        (P.succeed (\first second -> [ first, second ])
                            |= wordParser
                            |. P.token " "
                            |= wordParser
                        )
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "fails with an empty string" <|
            \() ->
                ""
                    |> P.run wordParser
                    |> Result.withDefault "eek"
                    |> Expect.equal "eek"
        , test "won't parse beyond the end of the line" <|
            \() ->
                "foo\nbar"
                    |> P.run
                        (P.succeed (\first second -> [ first, second ])
                            |= wordParser
                            |= wordParser
                        )
                    |> Result.withDefault [ "eek" ]
                    |> Expect.equal [ "eek" ]
        , test "won't parse beyond the end of the input" <|
            \() ->
                "foo"
                    |> P.run
                        (P.succeed (\first second -> [ first, second ])
                            |= wordParser
                            |= wordParser
                        )
                    |> Result.withDefault [ "eek" ]
                    |> Expect.equal [ "eek" ]
        ]
