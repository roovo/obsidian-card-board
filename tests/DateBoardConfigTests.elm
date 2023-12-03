module DateBoardConfigTests exposing (suite)

import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ encodeDecode
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleConfig
                    |> TsEncode.runExample DateBoardConfig.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder DateBoardConfig.decoder_v_0_11_0
                    |> .decoded
                    |> Expect.equal (Ok exampleConfig)
        ]



-- HELPERS


exampleConfig : DateBoardConfig
exampleConfig =
    BoardConfigHelpers.exampleDateBoardConfig
