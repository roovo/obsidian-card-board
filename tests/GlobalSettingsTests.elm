module GlobalSettingsTests exposing (suite)

import DefaultColumnNames
import Expect
import GlobalSettings exposing (GlobalSettings)
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ default
        , encodeDecode
        , toggleIgnoreFileNameDate
        , updateColumnName
        , updateTaskCompletionFormat
        ]


default : Test
default =
    describe "default"
        [ test "has ObsidianCardBoard update format" <|
            \() ->
                GlobalSettings.default
                    |> Expect.equal
                        { taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                        , defaultColumnNames = DefaultColumnNames.default
                        , ignoreFileNameDates = False
                        }
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding GlobalSettings"
        [ test "can decode an encoded string back to the original" <|
            \() ->
                { taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                , defaultColumnNames =
                    { today = Just "Do Today"
                    , tomorrow = Nothing
                    , future = Just "The Future"
                    , undated = Nothing
                    , otherTags = Just "The Others"
                    , untagged = Nothing
                    , completed = Just "Done"
                    }
                , ignoreFileNameDates = True
                }
                    |> TsEncode.runExample GlobalSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder GlobalSettings.v_0_11_0_decoder
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                            , defaultColumnNames =
                                { today = Just "Do Today"
                                , tomorrow = Nothing
                                , future = Just "The Future"
                                , undated = Nothing
                                , otherTags = Just "The Others"
                                , untagged = Nothing
                                , completed = Just "Done"
                                }
                            , ignoreFileNameDates = True
                            }
                        )
        ]


toggleIgnoreFileNameDate : Test
toggleIgnoreFileNameDate =
    describe "toggleIgnoreFileNameDate"
        [ test "can update a valid column name" <|
            \() ->
                GlobalSettings.default
                    |> GlobalSettings.toggleIgnoreFileNameDate
                    |> .ignoreFileNameDates
                    |> Expect.equal True
        ]


updateColumnName : Test
updateColumnName =
    describe "updateColumnName"
        [ test "can update a valid column name" <|
            \() ->
                GlobalSettings.default
                    |> GlobalSettings.updateColumnName "future" "Back to the"
                    |> .defaultColumnNames
                    |> Expect.equal
                        { today = Nothing
                        , tomorrow = Nothing
                        , future = Just "Back to the"
                        , undated = Nothing
                        , otherTags = Nothing
                        , untagged = Nothing
                        , completed = Nothing
                        }
        ]


updateTaskCompletionFormat : Test
updateTaskCompletionFormat =
    describe "updateTaskCompletionFormat"
        [ test "can update to be NoCompletion format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "NoCompletion"
                    |> .taskCompletionFormat
                    |> Expect.equal GlobalSettings.NoCompletion
        , test "can update to be ObsidianCardBoard format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> .taskCompletionFormat
                    |> Expect.equal GlobalSettings.ObsidianCardBoard
        , test "can update to be ObsidianDataview format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianDataview"
                    |> .taskCompletionFormat
                    |> Expect.equal GlobalSettings.ObsidianDataview
        , test "can update to be ObsidianTasks format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianTasks"
                    |> .taskCompletionFormat
                    |> Expect.equal GlobalSettings.ObsidianTasks
        , test "defaults to be ObsidianCardBoard if the string is not recognised" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianTasks"
                    |> GlobalSettings.updateTaskCompletionFormat "xxxxxx"
                    |> .taskCompletionFormat
                    |> Expect.equal GlobalSettings.ObsidianCardBoard
        ]



-- HELPERS


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskCompletionFormat = GlobalSettings.ObsidianTasks
    , defaultColumnNames = DefaultColumnNames.default
    , ignoreFileNameDates = False
    }
