module Form.UntaggedColumnTests exposing (suite)

import Column.Untagged as UntaggedColumn
import Expect
import Form.Decoder as FD
import Form.UntaggedColumn as UntaggedColumnForm
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
                    |> FD.run UntaggedColumnForm.decoder
                    |> Expect.equal (Ok <| UntaggedColumn.init "foo")
        , test "errors with an empty name" <|
            \() ->
                { name = "" }
                    |> FD.errors UntaggedColumnForm.decoder
                    |> Expect.equal [ UntaggedColumnForm.NameRequired ]
        ]
