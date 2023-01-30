module SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import CollapsedColumns
import ColumnNames
import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Filter
import GlobalSettings exposing (GlobalSettings)
import Helpers.FilterHelpers as FilterHelpers
import SafeZipper
import Settings exposing (Settings)
import SettingsState
import TagBoardConfig exposing (TagBoardConfig)
import Test exposing (..)


suite : Test
suite =
    concat
        [ addBoardRequested
        , boardConfigs
        , cancelCurrentState
        , confirmAddBoard
        , confirmDeleteBoard
        , deleteBoardRequested
        , editBoardAt
        , editGlobalSettings
        , init
        , mapBoardBeingAdded
        , mapBoardBeingEdited
        , mapGlobalSettings
        ]


addBoardRequested : Test
addBoardRequested =
    describe "addBoardRequested"
        [ test "does nothing if already in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default Settings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "ClosingPlugin -> AddingBoard" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "ClosingSettings -> AddingBoard" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "DeletingBoard -> AddingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "EditingBoard -> AddingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "EditingGlobalSettings -> AddingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        ]


boardConfigs : Test
boardConfigs =
    describe "boardConfigs"
        [ test "returns the configs (apart from the one being added) if in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "AddingBoard -> ClosingPlugin if the board list is empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default Settings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default)
        , test "AddingBoard -> EditingBoard if the board list is NOT empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ]))
        , test "ClosingPlugin -> ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default)
        , test "ClosingSettings -> ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard Settings.default)
        , test "EditingBoard -> ClosingSettings" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default)
        , test "EditingGlobalSettings -> ClosingSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default)
        ]


confirmAddBoard : Test
confirmAddBoard =
    describe "confirmAddBoard"
        [ test "AddingBoard -> EditingBoard focussed on the new board which is on the end" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigDateBoard (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ]))
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default)
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard Settings.default)
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        ]


confirmDeleteBoard : Test
confirmDeleteBoard =
    describe "confirmDeleteBoard"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default Settings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default)
        , test "deletes the current board and -> Adding if DeletingBoard and there is ONLY one board" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "deletes the current board and -> EditingBoard if DeletingBoard and there is MORE THAN one board" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigDateBoard ]))
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard Settings.default)
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        ]


deleteBoardRequested : Test
deleteBoardRequested =
    describe "deleteBoardRequested"
        [ test "AddingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default Settings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        , test "ClosingPlugin -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        , test "ClosingSettings -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        , test "does nothing if already DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        , test "EditingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        , test "EditingGlobalSettings -> DeletingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        ]


editBoardAt : Test
editBoardAt =
    describe "editBoardAt"
        [ test "AddingBoard -> EditingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "ClosingPlugin -> EditingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "ClosingSettings -> EditingBoard" <|
            \() ->
                SettingsState.ClosingSettings (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "EditingBoard -> EditingBoard (switched)" <|
            \() ->
                SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "EditingGlobalSettings -> EditingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        ]


editGlobalSettings : Test
editGlobalSettings =
    describe "editGlobalSettings"
        [ test "AddingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default Settings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        , test "ClosingPlugin -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        , test "ClosingSettings -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        , test "DeletingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.DeletingBoard Settings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        , test "EditingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        , test "does nothing to EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard with the default BoardConfig if there are no existing board configs" <|
            \() ->
                SettingsState.init Settings.default
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default Settings.default)
        , test "returns EditingBoard boardConfig" <|
            \() ->
                SettingsState.init (settingsFromBoardConfigs [ exampleBoardConfigDateBoard ])
                    |> Expect.equal (SettingsState.EditingBoard <| settingsFromBoardConfigs [ exampleBoardConfigDateBoard ])
        ]


mapBoardBeingAdded : Test
mapBoardBeingAdded =
    describe "mapBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default Settings.default
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigTagBoard Settings.default)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard Settings.default
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard Settings.default)
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default)
        ]


mapBoardBeingEdited : Test
mapBoardBeingEdited =
    describe "mapBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "updates the current board if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfigTagBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        ]


mapGlobalSettings : Test
mapGlobalSettings =
    describe "mapGlobalSettings"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default Settings.default
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings Settings.default
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard Settings.default
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard Settings.default
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "updates the current board if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal exampleGlobalSettings
        ]



-- HELPERS


settingsFromBoardConfigs : List BoardConfig -> Settings
settingsFromBoardConfigs boardConfigs_ =
    Settings.default
        |> Settings.updateBoardConfigs (SafeZipper.fromList boardConfigs_)


settingsFromBoardConfigsWithIndex : Int -> List BoardConfig -> Settings
settingsFromBoardConfigsWithIndex index boardConfigs_ =
    Settings.default
        |> Settings.updateBoardConfigs (SafeZipper.atIndex index <| SafeZipper.fromList boardConfigs_)


exampleBoardConfigDateBoard : BoardConfig
exampleBoardConfigDateBoard =
    BoardConfig.DateBoardConfig exampleDateBoardConfig


exampleBoardConfigTagBoard : BoardConfig
exampleBoardConfigTagBoard =
    BoardConfig.TagBoardConfig exampleTagBoardConfig


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskCompletionFormat = GlobalSettings.ObsidianTasks
    , columnNames = ColumnNames.default
    }


exampleDateBoardConfig : DateBoardConfig
exampleDateBoardConfig =
    { completedCount = 12
    , filters = []
    , filterPolarity = Filter.Deny
    , filterScope = Filter.SubTasksOnly
    , showFilteredTags = True
    , includeUndated = False
    , title = "Date Board Title"
    , collapsedColumns = CollapsedColumns.init
    }


exampleTagBoardConfig : TagBoardConfig
exampleTagBoardConfig =
    { columns = [ { tag = "foo", displayTitle = "bar" } ]
    , showColumnTags = True
    , completedCount = 6
    , filters = [ FilterHelpers.pathFilter "a", FilterHelpers.pathFilter "b", FilterHelpers.tagFilter "t1", FilterHelpers.tagFilter "t2" ]
    , filterPolarity = Filter.Deny
    , filterScope = Filter.TopLevelOnly
    , showFilteredTags = True
    , includeOthers = False
    , includeUntagged = True
    , title = "Tag Board Title"
    , collapsedColumns = CollapsedColumns.init
    }
