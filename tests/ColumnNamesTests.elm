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
        , updateColumnName
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


updateColumnName : Test
updateColumnName =
    describe "updateColumnName"
        [ test "updates a valid column name" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.updateColumnName "future" "Back to the"
                    |> Expect.equal
                        { today = Nothing
                        , tomorrow = Nothing
                        , future = Just "Back to the"
                        , undated = Nothing
                        , others = Nothing
                        , untagged = Nothing
                        , completed = Nothing
                        }
        , test "sets a column name to Nothing if it is an empty string" <|
            \() ->
                { today = Nothing
                , tomorrow = Nothing
                , future = Just "Back to the"
                , undated = Nothing
                , others = Nothing
                , untagged = Nothing
                , completed = Nothing
                }
                    |> ColumnNames.updateColumnName "future" ""
                    |> Expect.equal
                        ColumnNames.default
        , test "does nothing if the column name is not recognised" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.updateColumnName "xxx" "Back to the"
                    |> Expect.equal
                        { today = Nothing
                        , tomorrow = Nothing
                        , future = Nothing
                        , undated = Nothing
                        , others = Nothing
                        , untagged = Nothing
                        , completed = Nothing
                        }
        ]



-- HELPERS


exampleColumnNames : String
exampleColumnNames =
    """{"today":"col1","tomorrow":"col2","future":"col3","undated":"col4","others":"col4","untagged":"col6","completed":"col7"}"""
