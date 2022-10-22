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
                    |> Expect.equal { taskUpdateFormat = GlobalSettings.ObsidianCardBoard }
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding GlobalSettings"
        [ test "can decode an encoded string back to the original" <|
            \() ->
                { taskUpdateFormat = GlobalSettings.ObsidianCardBoard }
                    |> TsEncode.runExample GlobalSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder GlobalSettings.decoder
                    |> .decoded
                    |> Expect.equal (Ok { taskUpdateFormat = GlobalSettings.ObsidianCardBoard })
        ]


updateTaskUpdateFormat : Test
updateTaskUpdateFormat =
    describe "updateTaskUpdateFormat"
        [ test "can update to be ObsidianCardBoard format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> Expect.equal { taskUpdateFormat = GlobalSettings.ObsidianCardBoard }
        , test "can update to be ObsidianTasks format" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> Expect.equal { taskUpdateFormat = GlobalSettings.ObsidianTasks }
        , test "defaults to be ObsidianCardBoard if the string is not recognised" <|
            \() ->
                exampleGlobalSettings
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianCardBoard"
                    |> GlobalSettings.updateTaskUpdateFormat "ObsidianTasks"
                    |> GlobalSettings.updateTaskUpdateFormat "xxxxxx"
                    |> Expect.equal { taskUpdateFormat = GlobalSettings.ObsidianCardBoard }
        ]



-- HELPERS


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskUpdateFormat = GlobalSettings.ObsidianTasks }
