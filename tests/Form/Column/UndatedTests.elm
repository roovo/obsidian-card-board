module Form.Column.UndatedTests exposing (suite)

import Column.Undated as UndatedColumn
import Expect
import Form.Column.Undated as UndatedColumnForm
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
