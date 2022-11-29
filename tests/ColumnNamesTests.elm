module ColumnNamesTests exposing (suite)

import ColumnNames
import Expect
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
    describe "encodeDecode"
        [ test "can decode the encoded default back to the original" <|
            \() ->
                ColumnNames.default
                    |> TsEncode.runExample ColumnNames.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder ColumnNames.decoder
                    |> .decoded
                    |> Expect.equal (Ok ColumnNames.default)
        , test "can decode an encoded non default back to the original" <|
            \() ->
                exampleColumnNames
                    |> DecodeHelpers.runDecoder ColumnNames.decoder
                    |> .decoded
                    |> Result.withDefault ColumnNames.default
                    |> TsEncode.runExample ColumnNames.encoder
                    |> .output
                    |> Expect.equal exampleColumnNames
        ]


exampleColumnNames : String
exampleColumnNames =
    """{"today":"col1","tomorrow":"col2","future":"col3","undated":"col4","others":"col4","untagged":"col6","completed":"col7"}"""
