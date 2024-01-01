module GlobalSettingsTests exposing (suite)

import DefaultColumnNames
import Expect
import GlobalSettings
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ default
        , encodeDecode
        , taskCompletionSettings
        ]


default : Test
default =
    describe "default"
        [ test "has ObsidianCardBoard update format" <|
            \() ->
                GlobalSettings.default
                    |> Expect.equal
                        { defaultColumnNames = DefaultColumnNames.default
                        , ignoreFileNameDates = False
                        , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                        , taskCompletionInLocalTime = True
                        , taskCompletionWithUtcOffset = True
                        }
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding GlobalSettings"
        [ test "can decode an encoded string back to the original" <|
            \() ->
                { defaultColumnNames =
                    { today = Just "Do Today"
                    , tomorrow = Nothing
                    , future = Just "The Future"
                    , undated = Nothing
                    , otherTags = Just "The Others"
                    , untagged = Nothing
                    , completed = Just "Done"
                    }
                , ignoreFileNameDates = True
                , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                , taskCompletionInLocalTime = False
                , taskCompletionWithUtcOffset = False
                }
                    |> TsEncode.runExample GlobalSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder GlobalSettings.v_0_12_0_decoder
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { defaultColumnNames =
                                { today = Just "Do Today"
                                , tomorrow = Nothing
                                , future = Just "The Future"
                                , undated = Nothing
                                , otherTags = Just "The Others"
                                , untagged = Nothing
                                , completed = Just "Done"
                                }
                            , ignoreFileNameDates = True
                            , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                            , taskCompletionInLocalTime = False
                            , taskCompletionWithUtcOffset = False
                            }
                        )
        ]


taskCompletionSettings : Test
taskCompletionSettings =
    describe "taskCompletionSettings"
        [ test "returns the format, local time and utc offset settings" <|
            \() ->
                { defaultColumnNames = DefaultColumnNames.default
                , ignoreFileNameDates = False
                , taskCompletionFormat = GlobalSettings.ObsidianDataview
                , taskCompletionInLocalTime = True
                , taskCompletionWithUtcOffset = False
                }
                    |> GlobalSettings.taskCompletionSettings
                    |> Expect.equal
                        { format = GlobalSettings.ObsidianDataview
                        , inLocalTime = True
                        , showUtcOffset = False
                        }
        ]
