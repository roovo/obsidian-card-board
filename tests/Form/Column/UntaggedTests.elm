module Form.Column.UntaggedTests exposing (suite)

import Column.Untagged as UntaggedColumn
import Expect
import Form.Column.Untagged as UntaggedColumnForm
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
                { name = "foo" }
                    |> FD.run UntaggedColumnForm.decoder
                    |> Expect.equal (Ok <| UntaggedColumn.init "foo")
        , test "ignores leading and trailing whitespace when decoding a valid input" <|
            \() ->
                { name = " foo " }
                    |> FD.run UntaggedColumnForm.decoder
                    |> Expect.equal (Ok <| UntaggedColumn.init "foo")
        , test "errors with an empty name" <|
            \() ->
                { name = "" }
                    |> FD.errors UntaggedColumnForm.decoder
                    |> Expect.equal [ UntaggedColumnForm.NameRequired ]
        ]


init : Test
init =
    describe "init"
        [ test "initialises the name" <|
            \() ->
                UntaggedColumn.init "foo"
                    |> UntaggedColumnForm.init
                    |> .name
                    |> Expect.equal "foo"
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo" }
                    |> SD.run UntaggedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| UntaggedColumn.init "foo")
        , test "ignores leading and trailing whitespace when decoding a valid input" <|
            \() ->
                { name = " foo " }
                    |> SD.run UntaggedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| UntaggedColumn.init "foo")
        , test "errors with an empty name" <|
            \() ->
                { name = "" }
                    |> SD.run UntaggedColumnForm.safeDecoder
                    |> Expect.equal (Ok <| UntaggedColumn.init "")
        ]
