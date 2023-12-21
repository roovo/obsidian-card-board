module Form.NamedTagColumnTests exposing (suite)

import Column.NamedTag as NamedTagColumn
import Expect
import Form.Decoder as FD
import Form.NamedTagColumn as NamedTagColumnForm
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
                { name = "foo", tag = "aTag" }
                    |> FD.run NamedTagColumnForm.decoder
                    |> Expect.equal (Ok <| NamedTagColumn.init "foo" "aTag")
        , test "errors with an empty tag" <|
            \() ->
                { name = "foo", tag = "" }
                    |> FD.errors NamedTagColumnForm.decoder
                    |> Expect.equal [ NamedTagColumnForm.TagRequired ]
        , test "errors with tag containing invalid characters" <|
            \() ->
                { name = "foo", tag = "f$d" }
                    |> FD.errors NamedTagColumnForm.decoder
                    |> Expect.equal [ NamedTagColumnForm.InvalidTagCharacters ]
        , test "errors with tag containing whitespace" <|
            \() ->
                { name = "foo", tag = "aTag bTag" }
                    |> FD.errors NamedTagColumnForm.decoder
                    |> Expect.equal [ NamedTagColumnForm.InvalidTagCharacters ]
        , test "errors with an empty name" <|
            \() ->
                { name = "", tag = "aTag" }
                    |> FD.errors NamedTagColumnForm.decoder
                    |> Expect.equal [ NamedTagColumnForm.NameRequired ]
        ]
