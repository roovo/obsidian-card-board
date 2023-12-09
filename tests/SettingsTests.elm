module SettingsTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import DefaultColumnNames
import DragAndDrop.BeaconPosition as BeaconPosition
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
        [ blankBoardNames
        , cleanupNames
        , currentVersion
        , encodeDecode
        , moveBoard
        , uniqueBoardNames
        ]


blankBoardNames : Test
blankBoardNames =
    describe "handling boards with no names"
        [ test "gives boards with no names the name Unnamed" <|
            \() ->
                exampleSettingsWithMissingBoardNames
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Result.map .boardConfigs
                    |> Result.withDefault SafeZipper.empty
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "Unnamed", "B name", "Unnamed.2", "Unnamed.3" ]
        ]


cleanupNames : Test
cleanupNames =
    describe "cleanupNames"
        [ test "appends the index number of the board onto the end of a non-unique name" <|
            \() ->
                exampleSettingsWithDuplicatgeBoardNames
                    |> Settings.cleanupNames
                    |> .boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "A name", "B name", "A name.2", "A name.3" ]
        , test "maintians the current index when unique-ing" <|
            \() ->
                exampleSettingsWithDuplicatgeBoardNames
                    |> Settings.switchToBoard 2
                    |> Settings.cleanupNames
                    |> .boardConfigs
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 2)
        , test "gives boards with no names the name Unnamed" <|
            \() ->
                exampleSettingsWithMissingBoardNames
                    |> Settings.cleanupNames
                    |> .boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "Unnamed", "B name", "Unnamed.2", "Unnamed.3" ]
        ]


currentVersion : Test
currentVersion =
    describe "currentVersion"
        [ test "is 0.11.0" <|
            \() ->
                Settings.currentVersion
                    |> Semver.print
                    |> Expect.equal "0.11.0"
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
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "1", "2", "3", "4" ]
        , test "moves a board to the previous position using Before" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.Before "1")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "2", "1", "3", "4" ]
        , test "moves a board to the previous position using After" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "0")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "2", "1", "3", "4" ]
        , test "moves a board to the next position using Before" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.Before "4")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "1", "3", "2", "4" ]
        , test "moves a board to the next position using After" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "3")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "1", "3", "2", "4" ]
        , test "moves a board to the first position using Before" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.Before "0")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "2", "0", "1", "3", "4" ]
        , test "moves a board to the last position using After" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "4")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "1", "3", "4", "2" ]
        , test "sets the index to the moved board after moving" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "3")
                    |> Settings.boardConfigs
                    |> SafeZipper.current
                    |> Maybe.map BoardConfig.name
                    |> Expect.equal (Just "2")
        , test "sets the index to the moved board after moving even if position is the same" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "2")
                    |> Settings.boardConfigs
                    |> SafeZipper.current
                    |> Maybe.map BoardConfig.name
                    |> Expect.equal (Just "2")
        , test "returns the same list if the board to be moved does not exist" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "x" (BeaconPosition.After "2")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "1", "2", "3", "4" ]
        , test "returns the same list if the position to be moved to does not exist" <|
            \() ->
                exampleSettingsWithConsecutiveNames
                    |> Settings.moveBoard "2" (BeaconPosition.After "x")
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "0", "1", "2", "3", "4" ]
        ]


uniqueBoardNames : Test
uniqueBoardNames =
    describe "ensuring that board names are unique after decoding"
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
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "A name", "B name", "A name.2", "A name.3" ]
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
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "A name", "B name", "A_name.2", "A name.3" ]
        ]



-- HELPERS


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskCompletionFormat = GlobalSettings.ObsidianTasks
    , defaultColumnNames = DefaultColumnNames.default
    , ignoreFileNameDates = False
    }


exampleSettings : Settings.Settings
exampleSettings =
    { boardConfigs =
        SafeZipper.fromList
            [ BoardConfigHelpers.exampleTagBoardConfig
            , BoardConfigHelpers.exampleDateBoardConfig
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleSettingsWithConsecutiveNames : Settings.Settings
exampleSettingsWithConsecutiveNames =
    { boardConfigs =
        SafeZipper.fromList
            [ exampleTagBoardConfig |> BoardConfig.updateName "0"
            , exampleTagBoardConfig |> BoardConfig.updateName "1"
            , exampleTagBoardConfig |> BoardConfig.updateName "2"
            , exampleTagBoardConfig |> BoardConfig.updateName "3"
            , exampleTagBoardConfig |> BoardConfig.updateName "4"
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleSettingsWithDuplicatgeBoardNames : Settings.Settings
exampleSettingsWithDuplicatgeBoardNames =
    { boardConfigs =
        SafeZipper.fromList
            [ exampleTagBoardConfig |> BoardConfig.updateName "A name"
            , exampleTagBoardConfig |> BoardConfig.updateName "B name"
            , exampleTagBoardConfig |> BoardConfig.updateName "A name"
            , exampleTagBoardConfig |> BoardConfig.updateName "A name"
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleSettingsWithMissingBoardNames : Settings.Settings
exampleSettingsWithMissingBoardNames =
    { boardConfigs =
        SafeZipper.fromList
            [ exampleTagBoardConfig |> BoardConfig.updateName ""
            , exampleTagBoardConfig |> BoardConfig.updateName "B name"
            , exampleTagBoardConfig |> BoardConfig.updateName " "
            , exampleTagBoardConfig |> BoardConfig.updateName "   "
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleWithUnderscores : Settings.Settings
exampleWithUnderscores =
    { boardConfigs =
        SafeZipper.fromList
            [ exampleTagBoardConfig |> BoardConfig.updateName "A name"
            , exampleTagBoardConfig |> BoardConfig.updateName "B name"
            , exampleTagBoardConfig |> BoardConfig.updateName "A_name"
            , exampleTagBoardConfig |> BoardConfig.updateName "A name"
            ]
    , globalSettings = exampleGlobalSettings
    , version = Settings.currentVersion
    }


exampleTagBoardConfig : BoardConfig
exampleTagBoardConfig =
    BoardConfigHelpers.exampleTagBoardConfig
