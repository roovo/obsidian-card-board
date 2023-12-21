module Form.UndatedColumnTests exposing (suite)

import Column.Undated as UndatedColumn
import Expect
import Form.Decoder as FD
import Form.UndatedColumn as UndatedColumnForm
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , init
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo" }
                    |> FD.run UndatedColumnForm.decoder
                    |> Expect.equal (Ok <| UndatedColumn.init "foo")
        , test "errors with an empty name" <|
            \() ->
                { name = "" }
                    |> FD.errors UndatedColumnForm.decoder
                    |> Expect.equal [ UndatedColumnForm.NameRequired ]
        ]


init : Test
init =
    describe "init"
        [ test "initialises the name" <|
            \() ->
                UndatedColumn.init "foo"
                    |> UndatedColumnForm.init
                    |> .name
                    |> Expect.equal "foo"
        ]
