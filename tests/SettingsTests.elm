module SettingsTests exposing (suite)

import BoardConfig
import ColumnNames
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
        , uniqueBoardTitles
        ]


currentVersion : Test
currentVersion =
    describe "currentVersion"
        [ test "is 0.10.0" <|
            \() ->
                Settings.currentVersion
                    |> Semver.print
                    |> Expect.equal "0.10.0"
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


uniqueBoardTitles =
    describe "ensuring that board titles are unique after decoding"
        [ test "appends the index number of the board onto the end of a non-unique name" <|
            \() ->
                exampleSettingsWithDuplicatgeBoardNames
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Result.map .boardConfigs
                    |> Result.withDefault SafeZipper.empty
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "A title", "B title", "A title.2", "A title.3" ]
        , test "treats underscores and spaces as the same" <|
            \() ->
                exampleWithUnderscores
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Result.map .boardConfigs
                    |> Result.withDefault SafeZipper.empty
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "A title", "B title", "A_title.2", "A title.3" ]
        ]



-- HELPERS


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskCompletionFormat = GlobalSettings.ObsidianTasks
    , columnNames = ColumnNames.default
    , ignoreFileNameDates = False
    }


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


exampleSettingsWithDuplicatgeBoardNames : Settings.Settings
exampleSettingsWithDuplicatgeBoardNames =
    { boardConfigs =
        SafeZipper.fromList
            [ BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "A title" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "B title" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "A title" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "A title" }
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleWithUnderscores : Settings.Settings
exampleWithUnderscores =
    { boardConfigs =
        SafeZipper.fromList
            [ BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "A title" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "B title" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "A_title" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "A title" }
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleTagBoardConfig =
    BoardConfigHelpers.exampleTagBoardConfig
