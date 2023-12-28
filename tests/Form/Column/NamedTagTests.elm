module Form.Column.NamedTagTests exposing (suite)

import Column.NamedTag as NamedTagColumn
import Expect
import Form.Column.NamedTag as NamedTagColumnForm
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
                NamedTagColumn.init "foo" "aTag"
                    |> NamedTagColumnForm.init
                    |> .name
                    |> Expect.equal "foo"
        , test "initialises the tag" <|
            \() ->
                NamedTagColumn.init "foo" "aTag"
                    |> NamedTagColumnForm.init
                    |> .tag
                    |> Expect.equal "aTag"
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo", tag = "aTag" }
                    |> SD.run NamedTagColumnForm.safeDecoder
                    |> Expect.equal (Ok <| NamedTagColumn.init "foo" "aTag")
        , test "ignores leading and trailing whitespace when decoding a valid input" <|
            \() ->
                { name = " foo ", tag = " aTag " }
                    |> SD.run NamedTagColumnForm.safeDecoder
                    |> Expect.equal (Ok <| NamedTagColumn.init "foo" "aTag")
        , test "allows an empty tag" <|
            \() ->
                { name = "foo", tag = "" }
                    |> SD.run NamedTagColumnForm.safeDecoder
                    |> Expect.equal (Ok <| NamedTagColumn.init "foo" "")
        , test "allows a tag containing invalid characters" <|
            \() ->
                { name = "foo", tag = "f$d" }
                    |> SD.run NamedTagColumnForm.safeDecoder
                    |> Expect.equal (Ok <| NamedTagColumn.init "foo" "f$d")
        , test "allows with tag containing whitespace" <|
            \() ->
                { name = "foo", tag = "aTag bTag" }
                    |> SD.run NamedTagColumnForm.safeDecoder
                    |> Expect.equal (Ok <| NamedTagColumn.init "foo" "aTag bTag")
        , test "allows an empty name" <|
            \() ->
                { name = "", tag = "aTag" }
                    |> SD.run NamedTagColumnForm.safeDecoder
                    |> Expect.equal (Ok <| NamedTagColumn.init "" "aTag")
        ]
