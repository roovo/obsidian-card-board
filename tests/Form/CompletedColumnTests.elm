module Form.CompletedColumnTests exposing (suite)

import Column.Completed as CompletedColumn
import Expect
import Form.CompletedColumn as CompletedColumnForm
import Form.Decoder as FD
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo", limit = "7" }
                    |> FD.run CompletedColumnForm.decoder
                    |> Expect.equal (Ok <| CompletedColumn.init "foo" 0 7)
        , test "has no errors if the input is valid" <|
            \() ->
                { name = "foo", limit = "0" }
                    |> FD.errors CompletedColumnForm.decoder
                    |> Expect.equal []
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
                    |> Expect.equal [ CompletedColumnForm.LimitError CompletedColumnForm.Negative ]
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
