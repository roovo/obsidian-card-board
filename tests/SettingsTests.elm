module SettingsTests exposing (suite)

import BoardConfig
import ColumnNames
import DragAndDrop.BeaconPosition as BeaconPosition
import Expect
import GlobalSettings exposing (GlobalSettings)
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import SafeZipper
import Semver
import Settings
import TagBoardConfig exposing (TagBoardConfig)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ blankBoardTitles
        , cleanupTitles
        , currentVersion
        , encodeDecode
        , moveBoard
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


moveBoard : Test
moveBoard =
    describe "moveBoard"
        [ test "does nothing if there are no boards" <|
            \() ->
                { exampleSettings | boardConfigs = SafeZipper.empty }
                    |> Settings.moveBoard "0" (BeaconPosition.After "0")
                    |> Settings.boardConfigs
                    |> Expect.equal SafeZipper.empty
        , test "returns the same list if moving to the same index" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "2")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "1", "2", "3", "4" ]
        , test "moves a board to the previous position using Before" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.Before "1")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "2", "1", "3", "4" ]
        , test "moves a board to the previous position using After" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "0")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "2", "1", "3", "4" ]
        , test "moves a board to the next position using Before" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.Before "4")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "1", "3", "2", "4" ]
        , test "moves a board to the next position using After" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "3")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "1", "3", "2", "4" ]
        , test "moves a board to the first position using Before" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.Before "0")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "2", "0", "1", "3", "4" ]
        , test "moves a board to the last position using After" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "4")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "1", "3", "4", "2" ]
        , test "returns the same list if the board to be moved does not exist" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "x" (BeaconPosition.After "2")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "1", "2", "3", "4" ]
        , test "returns the same list if the position to be moved to does not exist" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "x")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.title
                    |> Expect.equal [ "0", "1", "2", "3", "4" ]
        ]


uniqueBoardTitles : Test
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


exampleSettingsWithConsecutiveNames : Settings.Settings
exampleSettingsWithConsecutiveNames =
    { boardConfigs =
        SafeZipper.fromList
            [ BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "0" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "1" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "2" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "3" }
            , BoardConfig.TagBoardConfig { exampleTagBoardConfig | title = "4" }
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


exampleTagBoardConfig : TagBoardConfig
exampleTagBoardConfig =
    BoardConfigHelpers.exampleTagBoardConfig
