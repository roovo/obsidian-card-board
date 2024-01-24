module GlobalSettingsTests exposing (suite)

import DefaultColumnNames
import Expect
import GlobalSettings
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)
import Time
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
                        , filters = []
                        , firstDayOfWeek = GlobalSettings.FromLocale
                        , ignoreFileNameDates = False
                        , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                        , taskCompletionInLocalTime = True
                        , taskCompletionShowUtcOffset = True
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
                , filters = []
                , firstDayOfWeek = GlobalSettings.SpecificWeekday Time.Wed
                , ignoreFileNameDates = True
                , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                , taskCompletionInLocalTime = False
                , taskCompletionShowUtcOffset = False
                }
                    |> TsEncode.runExample GlobalSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder GlobalSettings.v_0_13_0_decoder
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
                            , filters = []
                            , firstDayOfWeek = GlobalSettings.SpecificWeekday Time.Wed
                            , ignoreFileNameDates = True
                            , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                            , taskCompletionInLocalTime = False
                            , taskCompletionShowUtcOffset = False
                            }
                        )
        ]


taskCompletionSettings : Test
taskCompletionSettings =
    describe "taskCompletionSettings"
        [ test "returns the format, local time and utc offset settings" <|
            \() ->
                { defaultColumnNames = DefaultColumnNames.default
                , filters = []
                , firstDayOfWeek = GlobalSettings.FromLocale
                , ignoreFileNameDates = False
                , taskCompletionFormat = GlobalSettings.ObsidianDataview
                , taskCompletionInLocalTime = True
                , taskCompletionShowUtcOffset = False
                }
                    |> GlobalSettings.taskCompletionSettings
                    |> Expect.equal
                        { format = GlobalSettings.ObsidianDataview
                        , inLocalTime = True
                        , showUtcOffset = False
                        }
        ]
