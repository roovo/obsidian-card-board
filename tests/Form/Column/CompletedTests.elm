module Form.Column.CompletedTests exposing (suite)

import Column.Completed as CompletedColumn
import Expect
import Form.Column.Completed as CompletedColumnForm
import Form.SafeDecoder as SD
import Test exposing (..)


suite : Test
suite =
    concat
        [ init
        , safeDecoder
        ]


init : Test
init =
    describe "init"
        [ test "initialises the name" <|
            \() ->
                CompletedColumn.init "foo" 0 7
                    |> CompletedColumnForm.init
                    |> .name
                    |> Expect.equal "foo"
        , test "initialises the limit" <|
            \() ->
                CompletedColumn.init "foo" 0 7
                    |> CompletedColumnForm.init
                    |> .limit
                    |> Expect.equal "7"
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo", limit = "7" }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 7)
        , test "ignores leading and trailing whitespace when decoding a valid input" <|
            \() ->
                { name = "  foo  ", limit = " 7 " }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 7)
        , test "defaults to 10 with a 'x' as the limit" <|
            \() ->
                { name = "foo", limit = "x" }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 10)
        , test "defaults to 10 with a decimal limit" <|
            \() ->
                { name = "foo", limit = "1.1" }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 10)
        , test "defaults to 10 with a negative limit" <|
            \() ->
                { name = "foo", limit = "-1" }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 10)
        , test "defaults to 10 with a zero limit" <|
            \() ->
                { name = "foo", limit = "0" }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 10)
        , test "defaults to 10 with an empty limit" <|
            \() ->
                { name = "foo", limit = "" }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 10)
        , test "allows an empty name" <|
            \() ->
                { name = "", limit = "1" }
                    |> SD.run CompletedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| CompletedColumn.init "" 0 1)
        ]
