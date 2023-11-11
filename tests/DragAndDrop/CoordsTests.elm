module DragAndDrop.CoordsTests exposing (suite)

import DragAndDrop.Coords as Coords
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ decoder
        , distance
        , encoder
        , fromFloatTuple
        , subtract
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


distance : Test
distance =
    describe "distance"
        [ test "calculates the euclidean distance" <|
            \() ->
                { x = 5, y = 0 }
                    |> Coords.distance { x = 2, y = 2 }
                    |> Expect.within (Absolute 0.0001) (sqrt 13)
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


subtract : Test
subtract =
    describe "subtract"
        [ test "subtracts the x and y elements" <|
            \() ->
                { x = 2, y = 3 }
                    |> Coords.subtract { x = 5, y = 0 }
                    |> Expect.equal { x = 3, y = -3 }
        ]
