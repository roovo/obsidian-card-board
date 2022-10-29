module GlobalSettingsTests exposing (suite)

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
        , updateTaskCompletionFormat
        ]


default : Test
default =
    describe "default"
        [ test "has ObsidianCardBoard update format" <|
            \() ->
                GlobalSettings.default
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianCardBoard }
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding GlobalSettings"
        [ test "can decode an encoded string back to the original" <|
            \() ->
                { taskCompletionFormat = GlobalSettings.ObsidianCardBoard }
                    |> TsEncode.runExample GlobalSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder GlobalSettings.v_0_6_0_decoder
                    |> .decoded
                    |> Expect.equal (Ok { taskCompletionFormat = GlobalSettings.ObsidianCardBoard })
        ]


updateTaskCompletionFormat : Test
updateTaskCompletionFormat =
    describe "updateTaskCompletionFormat"
        [ test "can update to be NoCompletion format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "NoCompletion"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.NoCompletion }
        , test "can update to be ObsidianCardBoard format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianCardBoard }
        , test "can update to be ObsidianDataview format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianDataview"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianDataview }
        , test "can update to be ObsidianTasks format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianTasks"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianTasks }
        , test "defaults to be ObsidianCardBoard if the string is not recognised" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskCompletionFormat "ObsidianTasks"
                    |> GlobalSettings.updateTaskCompletionFormat "xxxxxx"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianCardBoard }
        ]



-- HELPERS


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskCompletionFormat = GlobalSettings.ObsidianTasks }
