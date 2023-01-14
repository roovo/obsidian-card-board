module DateBoardConfigTests exposing (suite)

import Column
import ColumnNames exposing (ColumnNames)
import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import List.Extra as LE
import TagList
import TaskItem
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
                    |> DecodeHelpers.runDecoder DateBoardConfig.decoder_v_0_5_0
                    |> .decoded
                    |> Expect.equal (Ok exampleConfig)
        ]



-- HELPERS


exampleConfig : DateBoardConfig
exampleConfig =
    BoardConfigHelpers.exampleDateBoardConfig
