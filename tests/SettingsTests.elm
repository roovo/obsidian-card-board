module SettingsTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import Column
import Column.Completed as CompletedColumn
import Columns
import DefaultColumnNames
import DragAndDrop.BeaconPosition as BeaconPosition
import Expect
import Filter
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
        [ cleanupNames
        , currentVersion
        , decoder
        , encodeDecode
        , moveBoard
        , uniqueBoardNames
        ]


cleanupNames : Test
cleanupNames =
    describe "cleanupNames"
        [ test "appends the index number of the board onto the end of a non-unique name" <|
            \() ->
                exampleSettingsWithDuplicateBoardNames
                    |> Settings.cleanupNames
                    |> .boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "A name", "B name", "A name.2", "A name.3" ]
        , test "maintians the current index when unique-ing" <|
            \() ->
                exampleSettingsWithDuplicateBoardNames
                    |> Settings.switchToBoard 2
                    |> Settings.cleanupNames
                    |> .boardConfigs
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 2)
        , test "gives boards with no names the name Unnamed" <|
            \() ->
                { boardConfigs =
                    SafeZipper.fromList
                        [ exampleTagBoardConfig |> BoardConfig.updateName ""
                        , exampleTagBoardConfig |> BoardConfig.updateName "B name"
                        , exampleTagBoardConfig |> BoardConfig.updateName " "
                        , exampleTagBoardConfig |> BoardConfig.updateName "   "
                        ]
                , globalSettings = GlobalSettings.default
                , version = Settings.currentVersion
                }
                    |> Settings.cleanupNames
                    |> .boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "Unnamed", "B name", "Unnamed.2", "Unnamed.3" ]
        , test "suffixes duplicate board names with the column number" <|
            \() ->
                { boardConfigs =
                    SafeZipper.fromList
                        [ exampleTagBoardConfig |> BoardConfig.updateName "foo"
                        , exampleTagBoardConfig |> BoardConfig.updateName "B name"
                        , exampleTagBoardConfig |> BoardConfig.updateName " foo "
                        , exampleTagBoardConfig |> BoardConfig.updateName "   foo"
                        ]
                , globalSettings = GlobalSettings.default
                , version = Settings.currentVersion
                }
                    |> Settings.cleanupNames
                    |> .boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "foo", "B name", "foo.2", "foo.3" ]
        ]


currentVersion : Test
currentVersion =
    describe "currentVersion"
        [ test "is 0.12.0" <|
            \() ->
                Settings.currentVersion
                    |> Semver.print
                    |> Expect.equal "0.12.0"
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "cleans up board names" <|
            \() ->
                { boardConfigs =
                    SafeZipper.fromList
                        [ exampleTagBoardConfig |> BoardConfig.updateName ""
                        , exampleTagBoardConfig |> BoardConfig.updateName "B name"
                        , exampleTagBoardConfig |> BoardConfig.updateName " "
                        , exampleTagBoardConfig |> BoardConfig.updateName " foo  "
                        , exampleTagBoardConfig |> BoardConfig.updateName "foo"
                        ]
                , globalSettings = exampleGlobalSettings
                , version = Settings.currentVersion
                }
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Result.map .boardConfigs
                    |> Result.withDefault SafeZipper.empty
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "Unnamed", "B name", "Unnamed.2", "foo", "foo.4" ]
        , test "cleans up columns" <|
            \() ->
                { boardConfigs =
                    SafeZipper.fromList
                        [ BoardConfig.BoardConfig
                            { columns =
                                Columns.fromList
                                    [ Column.untagged "Untagged"
                                    , Column.untagged "Untagged too"
                                    , Column.otherTags "Others" [ "foo" ]
                                    , Column.namedTag "foo" "foo"
                                    , Column.namedTag " foo" "foo"
                                    , Column.namedTag "" "foo"
                                    , Column.namedTag "  " "foo"
                                    , Column.completed <| CompletedColumn.init "Completed" 2 6
                                    , Column.completed <| CompletedColumn.init "Completed too" 2 6
                                    ]
                            , filters = []
                            , filterPolarity = Filter.Deny
                            , filterScope = Filter.SubTasksOnly
                            , showColumnTags = True
                            , showFilteredTags = False
                            , name = "Tag Board Name"
                            }
                        ]
                , globalSettings = exampleGlobalSettings
                , version = Settings.currentVersion
                }
                    |> TsEncode.runExample Settings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder Settings.decoder
                    |> .decoded
                    |> Result.map .boardConfigs
                    |> Result.withDefault SafeZipper.empty
                    |> SafeZipper.toList
                    |> List.head
                    |> Maybe.map BoardConfig.columns
                    |> Maybe.map Columns.toList
                    |> Maybe.withDefault []
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "Others", "foo", "foo.3", "Unnamed", "Unnamed.5", "Completed" ]
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
                exampleSettingsWithDuplicateBoardNames
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
    { defaultColumnNames = DefaultColumnNames.default
    , filters = []
    , ignoreFileNameDates = False
    , taskCompletionFormat = GlobalSettings.NoCompletion
    , taskCompletionInLocalTime = False
    , taskCompletionShowUtcOffset = False
    }


exampleSettings : Settings.Settings
exampleSettings =
    { boardConfigs = SafeZipper.fromList []
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


exampleSettingsWithDuplicateBoardNames : Settings.Settings
exampleSettingsWithDuplicateBoardNames =
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
