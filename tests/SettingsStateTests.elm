module SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import CardBoardSettings
import DateBoard
import Expect
import Filter
import Helpers.FilterHelpers as FilterHelpers
import SafeZipper exposing (SafeZipper)
import SettingsState
import TagBoard
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
        , init
        , switchBoardBeingEdited
        , updateBoardBeingAdded
        , updateBoardBeingEdited
        ]


addBoardRequested : Test
addBoardRequested =
    describe "addBoardRequested"
        [ test "does nothing if already in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "ClosingPlugin -> AddingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "ClosingSettings -> AddingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "DeletingBoard -> AddingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "EditingBoard -> AddingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        ]


boardConfigs : Test
boardConfigs =
    describe "boardConfigs"
        [ test "returns the configs (apart from the one being added) if in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "AddingBoard -> ClosingPlugin if the board list is empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default SafeZipper.empty CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin SafeZipper.empty CardBoardSettings.defaultGlobalSettings)
        , test "AddingBoard -> EditingBoard if the board list is NOT empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "ClosingPlugin -> ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin SafeZipper.empty CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin SafeZipper.empty CardBoardSettings.defaultGlobalSettings)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard SafeZipper.empty CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard SafeZipper.empty CardBoardSettings.defaultGlobalSettings)
        , test "EditingBoard -> ClosingSettings" <|
            \() ->
                SettingsState.EditingBoard SafeZipper.empty CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings SafeZipper.empty CardBoardSettings.defaultGlobalSettings)
        , test "ClosingSettings -> ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings SafeZipper.empty CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings SafeZipper.empty CardBoardSettings.defaultGlobalSettings)
        ]


confirmAddBoard : Test
confirmAddBoard =
    describe "confirmAddBoard"
        [ test "AddingBoard -> EditingBoard focussed on the new board which is on the end" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        ]


confirmDeleteBoard : Test
confirmDeleteBoard =
    describe "confirmDeleteBoard"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "deletes the current board and -> Adding if DeletingBoard and there is ONLY one board" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default SafeZipper.empty CardBoardSettings.defaultGlobalSettings)
        , test "deletes the current board and -> EditingBoard if DeletingBoard and there is MORE THAN one board" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.next <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.next <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigDateBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        ]


deleteBoardRequested : Test
deleteBoardRequested =
    describe "deleteBoardRequested"
        [ test "AddingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "ClosingPlugin -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "ClosingSettings -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if already DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "EditingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard SafeZipper.empty BoardConfig.default" <|
            \() ->
                SettingsState.init SafeZipper.empty CardBoardSettings.defaultGlobalSettings
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default SafeZipper.empty CardBoardSettings.defaultGlobalSettings)
        , test "returns EditingBoard boardConfig" <|
            \() ->
                SettingsState.init exampleBoardConfigsDateBoard CardBoardSettings.defaultGlobalSettings
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsDateBoard CardBoardSettings.defaultGlobalSettings)
        ]


switchBoardBeingEdited : Test
switchBoardBeingEdited =
    describe "switchBoardBeingEdited"
        [ test "AddingBoard -> EditingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "ClosingPlugin -> EditingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "ClosingSettings -> EditingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "EditingBoard -> EditingBoard (switched)" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        ]


updateBoardBeingAdded : Test
updateBoardBeingAdded =
    describe "updateBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigTagBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        ]


updateBoardBeingEdited : Test
updateBoardBeingEdited =
    describe "updateBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        , test "updates the current board if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard, exampleBoardConfigTagBoard ]) CardBoardSettings.defaultGlobalSettings)
        ]



-- HELPERS


exampleBoardConfigDateBoard : BoardConfig
exampleBoardConfigDateBoard =
    BoardConfig.DateBoardConfig exampleDateBoardConfig


exampleBoardConfigTagBoard : BoardConfig
exampleBoardConfigTagBoard =
    BoardConfig.TagBoardConfig exampleTagBoardConfig


exampleBoardConfigsDateBoard : SafeZipper BoardConfig
exampleBoardConfigsDateBoard =
    SafeZipper.fromList <| [ exampleBoardConfigDateBoard ]


exampleDateBoardConfig : DateBoard.Config
exampleDateBoardConfig =
    { completedCount = 12
    , filters = []
    , filterPolarity = Filter.Deny
    , showFilteredTags = True
    , includeUndated = False
    , title = "Date Board Title"
    }


exampleTagBoardConfig : TagBoard.Config
exampleTagBoardConfig =
    { columns = [ { tag = "foo", displayTitle = "bar" } ]
    , showColumnTags = True
    , completedCount = 6
    , filters = [ FilterHelpers.pathFilter "a", FilterHelpers.pathFilter "b", FilterHelpers.tagFilter "t1", FilterHelpers.tagFilter "t2" ]
    , filterPolarity = Filter.Deny
    , showFilteredTags = True
    , includeOthers = False
    , includeUntagged = True
    , title = "Tag Board Title"
    }
