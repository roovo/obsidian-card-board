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
