module BoardConfigTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import TagBoard
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ default
        , encodeDecode

        -- , toggleIncludeOthers
        -- , toggleIncludeUndated
        -- , toggleIncludeUntagged
        -- , updateBoardType
        -- , updateCompletedCount
        -- , updateTags
        -- , updateTitle
        ]


default : Test
default =
    describe "default"
        [ test "is for a TagBoard" <|
            \() ->
                BoardConfig.default
                    |> BoardConfig.isForTagBoard
                    |> Expect.equal True
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode an encoded string back to the original" <|
            \() ->
                BoardConfigHelpers.exampleBoardConfig
                    |> TsEncode.runExample BoardConfig.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder BoardConfig.decoder
                    |> .decoded
                    |> Expect.equal (Ok BoardConfigHelpers.exampleBoardConfig)
        ]
