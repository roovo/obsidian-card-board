module Form.InputTests exposing (suite)

import Expect
import Form.Decoder as FD
import Form.Input as FormInput
import Test exposing (..)


suite : Test
suite =
    concat
        [ required
        ]


required : Test
required =
    describe "required"
        [ test "decodes a non-empty input" <|
            \() ->
                FD.identity
                    |> FormInput.required "eek"
                    |> (\d -> FD.run d "foo")
                    |> Expect.equal (Ok "foo")
        , test "errors an empty input" <|
            \() ->
                FD.identity
                    |> FormInput.required "eek"
                    |> (\d -> FD.run d "")
                    |> Expect.equal (Err [ "eek" ])
        ]
