module TagTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Parser
import Tag
import Test exposing (..)


suite : Test
suite =
    concat
        [ parser
        ]


parser : Test
parser =
    describe "parser"
        [ test "fails with an empty string" <|
            \() ->
                ""
                    |> Parser.run Tag.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails with just a '#'" <|
            \() ->
                "#"
                    |> Parser.run Tag.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , fuzz validTagContentFuzzer "fails with strings that don't start with '#'" <|
            \fuzzedTag ->
                fuzzedTag
                    |> Parser.run Tag.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , fuzz stringWithInvalidCharacters "fails for tags containing invalid characters" <|
            \fuzzedTag ->
                ("#" ++ fuzzedTag)
                    |> Parser.run Tag.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , fuzz numericStringFuzzer "fails for tags containing only digits" <|
            \fuzzedTag ->
                ("#" ++ fuzzedTag)
                    |> Parser.run Tag.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , fuzz validTagContentFuzzer "parses '#foo-bar'" <|
            \fuzzedTag ->
                "#foo-bar"
                    |> Parser.run Tag.parser
                    |> Result.map Tag.toString
                    |> Expect.equal (Ok "foo-bar")
        , fuzz validTagContentFuzzer "parses '#foo_bar'" <|
            \fuzzedTag ->
                "#foo_bar"
                    |> Parser.run Tag.parser
                    |> Result.map Tag.toString
                    |> Expect.equal (Ok "foo_bar")
        , fuzz validTagContentFuzzer "parses '#foo/bar'" <|
            \fuzzedTag ->
                "#foo/bar"
                    |> Parser.run Tag.parser
                    |> Result.map Tag.toString
                    |> Expect.equal (Ok "foo/bar")
        , fuzz validTagContentFuzzer "parses all valid tags that start with '#'" <|
            \fuzzedTag ->
                ("#" ++ fuzzedTag)
                    |> Parser.run Tag.parser
                    |> Result.map Tag.toString
                    |> Expect.equal (Ok fuzzedTag)
        ]


stringWithInvalidCharacters : Fuzzer String
stringWithInvalidCharacters =
    let
        dropValidCharacters : String -> String
        dropValidCharacters a =
            String.toList a
                |> List.filter (not << isValidTagCharacter)
                |> String.fromList

        ensureNotEmpty : String -> String
        ensureNotEmpty a =
            if String.length a == 0 then
                "!"

            else
                a
    in
    Fuzz.string
        |> Fuzz.map dropValidCharacters
        |> Fuzz.map ensureNotEmpty


numericStringFuzzer : Fuzzer String
numericStringFuzzer =
    Fuzz.map String.fromInt Fuzz.int


validTagContentFuzzer : Fuzzer String
validTagContentFuzzer =
    let
        dropInvalidCharacters : String -> String
        dropInvalidCharacters a =
            String.toList a
                |> List.filter isValidTagCharacter
                |> String.fromList

        ensureNotEmpty : String -> String
        ensureNotEmpty a =
            if String.length a == 0 then
                "a"

            else
                a

        ensureNotNumeric : String -> String
        ensureNotNumeric a =
            case String.toInt a of
                Just _ ->
                    "a"

                Nothing ->
                    a
    in
    Fuzz.string
        |> Fuzz.map dropInvalidCharacters
        |> Fuzz.map ensureNotEmpty
        |> Fuzz.map ensureNotNumeric


isValidTagCharacter : Char -> Bool
isValidTagCharacter c =
    if Char.isAlphaNum c then
        True

    else if List.member c [ '_', '-', '/' ] then
        True

    else
        False
