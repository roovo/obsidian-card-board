module Form.Column.OtherTagsTests exposing (suite)

import Column.OtherTags as OtherTagsColumn
import Expect
import Form.Column.OtherTags as OtherTagsColumnForm
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
                OtherTagsColumn.init "foo" []
                    |> OtherTagsColumnForm.init
                    |> .name
                    |> Expect.equal "foo"
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes a valid input" <|
            \() ->
                { name = "foo" }
                    |> SD.run OtherTagsColumnForm.safeDecoder
                    |> Expect.equal (Ok <| OtherTagsColumn.init "foo" [])
        , test "ignores leading and trailing whitespace when decoding a valid input" <|
            \() ->
                { name = " foo " }
                    |> SD.run OtherTagsColumnForm.safeDecoder
                    |> Expect.equal (Ok <| OtherTagsColumn.init "foo" [])
        , test "allows an empty name" <|
            \() ->
                { name = "" }
                    |> SD.run OtherTagsColumnForm.safeDecoder
                    |> Expect.equal (Ok <| OtherTagsColumn.init "" [])
        ]
