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
        , fuzz noHashAtStartFuzzer "fails with strings that don't start with '#'" <|
            \fuzzedTag ->
                fuzzedTag
                    |> Parser.run Tag.parser
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , fuzz noHashAtStartFuzzer "parses strings that start with '#'" <|
            \fuzzedTag ->
                ("#" ++ fuzzedTag)
                    |> Parser.run Tag.parser
                    |> Result.map Tag.toString
                    |> Expect.equal (Ok fuzzedTag)
        ]


noHashAtStartFuzzer : Fuzzer String
noHashAtStartFuzzer =
    let
        dropWhileHashAtStart : String -> String
        dropWhileHashAtStart a =
            if String.startsWith "#" a then
                dropWhileHashAtStart <| String.dropLeft 1 a

            else
                a

        ensureNotEmpty : String -> String
        ensureNotEmpty a =
            if String.length a == 0 then
                "a"

            else
                a
    in
    Fuzz.string
        |> Fuzz.map dropWhileHashAtStart
        |> Fuzz.map ensureNotEmpty
