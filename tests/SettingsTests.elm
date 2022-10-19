module SettingsTests exposing (suite)

import BoardConfig
import Expect
import GlobalSettings exposing (GlobalSettings)
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import SafeZipper
import Semver
import Settings
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ currentVersion
        , encodeDecode
        ]


currentVersion : Test
currentVersion =
    describe "currentVersion"
        [ test "is 0.5.0" <|
            \() ->
                Settings.currentVersion
                    |> Semver.print
                    |> Expect.equal "0.5.0"
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding settings"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleSettings
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Expect.equal (Ok exampleSettings)
        , test "fails if the version number is unsupported" <|
            \() ->
                { exampleSettings | version = Semver.version 0 0 0 [] [] }
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]



-- HELPERS


exampleSettings : Settings.Settings
exampleSettings =
    { boardConfigs =
        SafeZipper.fromList
            [ BoardConfig.TagBoardConfig BoardConfigHelpers.exampleTagBoardConfig
            , BoardConfig.DateBoardConfig BoardConfigHelpers.exampleDateBoardConfig
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskUpdateFormat = GlobalSettings.ObsidianTasks }
