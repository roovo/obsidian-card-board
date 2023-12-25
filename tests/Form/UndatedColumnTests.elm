module Form.UndatedColumnTests exposing (suite)

import Column.Undated as UndatedColumn
import Expect
import Form.Decoder as FD
import Form.SafeDecoder as SD
import Form.UndatedColumn as UndatedColumnForm
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
                { name = "foo" }
                    |> FD.run UndatedColumnForm.decoder
                    |> Expect.equal (Ok <| UndatedColumn.init "foo")
        , test "ignores leading and trailing whitespacw when decoding a valid input" <|
            \() ->
                { name = " foo " }
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


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo" }
                    |> SD.run UndatedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| UndatedColumn.init "foo")
        , test "ignores leading and trailing whitespacw when decoding a valid input" <|
            \() ->
                { name = " foo " }
                    |> SD.run UndatedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| UndatedColumn.init "foo")
        , test "errors with an empty name" <|
            \() ->
                { name = "" }
                    |> SD.run UndatedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| UndatedColumn.init "")
        ]
