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
        , updateTaskUpdateFormat
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
                    |> DecodeHelpers.runDecoder GlobalSettings.decoder
                    |> .decoded
                    |> Expect.equal (Ok { taskCompletionFormat = GlobalSettings.ObsidianCardBoard })
        ]


updateTaskUpdateFormat : Test
updateTaskUpdateFormat =
    describe "updateTaskUpdateFormat"
        [ test "can update to be ObsidianCardBoard format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianCardBoard }
        , test "can update to be ObsidianDataview format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianDataview"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianDataview }
        , test "can update to be ObsidianTasks format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianTasks }
        , test "defaults to be ObsidianCardBoard if the string is not recognised" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> GlobalSettings.updateTaskUpdateFormat "xxxxxx"
                    |> Expect.equal { taskCompletionFormat = GlobalSettings.ObsidianCardBoard }
        ]



-- HELPERS


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskCompletionFormat = GlobalSettings.ObsidianTasks }
