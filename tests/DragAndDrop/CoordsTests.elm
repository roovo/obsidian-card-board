module DragAndDrop.CoordsTests exposing (suite)

import DragAndDrop.Coords as Coords
import Expect
import Fuzz exposing (Fuzzer)
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ decoder
        , encoder
        , fromFloatTuple
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


encoder : Test
encoder =
    describe "encoder"
        [ test "encoder valid input" <|
            \() ->
                { x = 1.1, y = 2.2 }
                    |> TsEncode.runExample Coords.encoder
                    |> .output
                    |> Expect.equal """{"x":1.1,"y":2.2}"""
        ]


fromFloatTuple : Test
fromFloatTuple =
    describe "fromFloatTuple"
        [ test "builds from a tuple of floats" <|
            \() ->
                ( 1.1, 2.2 )
                    |> Coords.fromFloatTuple
                    |> Expect.equal { x = 1.1, y = 2.2 }
        ]
