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
        [ test "fails with just a '#'" <|
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
        ]


noHashAtStartFuzzer : Fuzzer String
noHashAtStartFuzzer =
    let
        removeHashAtStart : String -> String
        removeHashAtStart a =
            if String.startsWith "#" a then
                String.dropLeft 1 a

            else
                a
    in
    Fuzz.string
