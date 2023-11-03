module DragAndDrop.CoordsTests exposing (suite)

import DragAndDrop.Coords as Coords
import Expect
import Fuzz exposing (Fuzzer)
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes valid input" <|
            \() ->
                """{"x":1.1,"y":2.2}"""
                    |> DecodeHelpers.runDecoder Coords.decoder
                    |> .decoded
                    |> Expect.equal (Ok <| { x = 1.1, y = 2.2 })
        ]
