module TaskPaperTagTests exposing (suite)

import Expect
import Parser
import TaskPaperTag
import Test exposing (..)


suite : Test
suite =
    concat
        [ parser
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
