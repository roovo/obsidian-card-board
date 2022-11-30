module ColumnNamesTests exposing (suite)

import ColumnNames exposing (ColumnNames)
import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ encodeDecode
        , nameFor
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


nameFor : Test
nameFor =
    describe "nameFor"
        [ test "default for today is Today" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.nameFor "today"
                    |> Expect.equal "Today"
        , test "default for tomorrow is Tomorrow" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.nameFor "tomorrow"
                    |> Expect.equal "Tomorrow"
        , test "default for future is Future" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.nameFor "future"
                    |> Expect.equal "Future"
        , test "default for undated is Undated" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.nameFor "undated"
                    |> Expect.equal "Undated"
        , test "default for others is Others" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.nameFor "others"
                    |> Expect.equal "Others"
        , test "default for untagged is Untagged" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.nameFor "untagged"
                    |> Expect.equal "Untagged"
        , test "default for completed is Completed" <|
            \() ->
                ColumnNames.default
                    |> ColumnNames.nameFor "completed"
                    |> Expect.equal "Completed"
        , test "can set the value for today" <|
            \() ->
                { defaultNames | today = Just "Foo" }
                    |> ColumnNames.nameFor "today"
                    |> Expect.equal "Foo"
        , test "can set the value for tomorrow" <|
            \() ->
                { defaultNames | tomorrow = Just "Foo" }
                    |> ColumnNames.nameFor "tomorrow"
                    |> Expect.equal "Foo"
        , test "can set the value for future" <|
            \() ->
                { defaultNames | future = Just "Foo" }
                    |> ColumnNames.nameFor "future"
                    |> Expect.equal "Foo"
        , test "can set the value for undated" <|
            \() ->
                { defaultNames | undated = Just "Foo" }
                    |> ColumnNames.nameFor "undated"
                    |> Expect.equal "Foo"
        , test "can set the value for others" <|
            \() ->
                { defaultNames | others = Just "Foo" }
                    |> ColumnNames.nameFor "others"
                    |> Expect.equal "Foo"
        , test "can set the value for untagged" <|
            \() ->
                { defaultNames | untagged = Just "Foo" }
                    |> ColumnNames.nameFor "untagged"
                    |> Expect.equal "Foo"
        , test "can set the value for completed" <|
            \() ->
                { defaultNames | completed = Just "Foo" }
                    |> ColumnNames.nameFor "completed"
                    |> Expect.equal "Foo"
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


defaultNames : ColumnNames
defaultNames =
    ColumnNames.default


exampleColumnNames : String
exampleColumnNames =
    """{"today":"col1","tomorrow":"col2","future":"col3","undated":"col4","others":"col4","untagged":"col6","completed":"col7"}"""
