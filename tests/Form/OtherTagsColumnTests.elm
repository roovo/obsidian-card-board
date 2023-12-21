module Form.OtherTagsColumnTests exposing (suite)

import Column.OtherTags as OtherTagsColumn
import Expect
import Form.Decoder as FD
import Form.OtherTagsColumn as OtherTagsColumnForm
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
                { name = "foo" }
                    |> FD.run OtherTagsColumnForm.decoder
                    |> Expect.equal (Ok <| OtherTagsColumn.init "foo" [])
        , test "errors with an empty name" <|
            \() ->
                { name = "" }
                    |> FD.errors OtherTagsColumnForm.decoder
                    |> Expect.equal [ OtherTagsColumnForm.NameRequired ]
        ]
