module Form.CompletedColumnTests exposing (suite)

import Column.Completed as CompletedColumn
import Expect
import Form.CompletedColumn as CompletedColumnForm
import Form.Decoder as FD
import Form.SafeDecoder as SD
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , init
        , safeDecoder
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo", limit = "7" }
                    |> FD.run CompletedColumnForm.decoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 7)
        , test "ignores leading and trailing whitespace when decoding a valid input" <|
            \() ->
                { name = "  foo  ", limit = " 7 " }
                    |> FD.run CompletedColumnForm.decoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 7)
        , test "errors with a 'x' as the limit" <|
            \() ->
                { name = "foo", limit = "x" }
                    |> FD.errors CompletedColumnForm.decoder
                    |> Expect.equal [ CompletedColumnForm.LimitError CompletedColumnForm.InvalidInt ]
        , test "errors with a decimal limit" <|
            \() ->
                { name = "foo", limit = "1.1" }
                    |> FD.errors CompletedColumnForm.decoder
                    |> Expect.equal [ CompletedColumnForm.LimitError CompletedColumnForm.InvalidInt ]
        , test "errors with a negative limit" <|
            \() ->
                { name = "foo", limit = "-1" }
                    |> FD.errors CompletedColumnForm.decoder
                    |> Expect.equal [ CompletedColumnForm.LimitError CompletedColumnForm.NotPositive ]
        , test "errors with a zero limit" <|
            \() ->
                { name = "foo", limit = "0" }
                    |> FD.errors CompletedColumnForm.decoder
                    |> Expect.equal [ CompletedColumnForm.LimitError CompletedColumnForm.NotPositive ]
        , test "errors with an empty limit" <|
            \() ->
                { name = "foo", limit = "" }
                    |> FD.errors CompletedColumnForm.decoder
                    |> Expect.equal [ CompletedColumnForm.LimitRequired ]
        , test "errors with an empty name" <|
            \() ->
                { name = "", limit = "1" }
                    |> FD.errors CompletedColumnForm.decoder
                    |> Expect.equal [ CompletedColumnForm.NameRequired ]
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
