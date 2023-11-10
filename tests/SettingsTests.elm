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
        [ blankBoardTitles
        , cleanupTitles
        , currentVersion
        , encodeDecode
        , uniqueBoardTitles
        ]


blankBoardTitles : Test
blankBoardTitles =
    describe "handling boards with no titles"
        [ test "gives boards with no titles the title Untitled" <|
            \() ->
                exampleSettingsWithMissingBoardNames
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Result.map .boardConfigs
                    |> Result.withDefault SafeZipper.empty
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "Untitled", "B title", "Untitled.2", "Untitled.3" ]
        ]


cleanupTitles : Test
cleanupTitles =
    describe "cleanupTitles"
        [ test "appends the index number of the board onto the end of a non-unique name" <|
            \() ->
                exampleSettingsWithDuplicatgeBoardNames
                    |> Settings.cleanupTitles
                    |> .boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "A title", "B title", "A title.2", "A title.3" ]
        , test "maintians the current index when unique-ing" <|
            \() ->
                exampleSettingsWithDuplicatgeBoardNames
                    |> Settings.switchToBoard 2
                    |> Settings.cleanupTitles
                    |> .boardConfigs
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 2)
        , test "gives boards with no titles the title Untitled" <|
            \() ->
                exampleSettingsWithMissingBoardNames
                    |> Settings.cleanupTitles
                    |> .boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "Untitled", "B title", "Untitled.2", "Untitled.3" ]
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


exampleSettingsWithMissingBoardNames : Settings.Settings
exampleSettingsWithMissingBoardNames =
    { boardConfigs =
        SafeZipper.fromList
            [ BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "B title" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = " " }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "   " }
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
