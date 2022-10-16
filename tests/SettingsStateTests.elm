module SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import CardBoardSettings
import DateBoard
import Expect
import Filter
import GlobalSettings exposing (GlobalSettings)
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
        , editBoardAt
        , editGlobalSettings
        , globalSettings
        , init
        , updateBoardBeingAdded
        , updateBoardBeingEdited
        , updateGlobalSettings
        ]


addBoardRequested : Test
addBoardRequested =
    describe "addBoardRequested"
        [ test "does nothing if already in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingPlugin -> AddingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingSettings -> AddingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "DeletingBoard -> AddingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "EditingBoard -> AddingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "EditingGlobalSettings -> AddingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


boardConfigs : Test
boardConfigs =
    describe "boardConfigs"
        [ test "returns the configs (apart from the one being added) if in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "AddingBoard -> ClosingPlugin if the board list is empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default SafeZipper.empty GlobalSettings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin SafeZipper.empty GlobalSettings.default)
        , test "AddingBoard -> EditingBoard if the board list is NOT empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingPlugin -> ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin SafeZipper.empty GlobalSettings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin SafeZipper.empty GlobalSettings.default)
        , test "ClosingSettings -> ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings SafeZipper.empty GlobalSettings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings SafeZipper.empty GlobalSettings.default)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard SafeZipper.empty GlobalSettings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard SafeZipper.empty GlobalSettings.default)
        , test "EditingBoard -> ClosingSettings" <|
            \() ->
                SettingsState.EditingBoard SafeZipper.empty GlobalSettings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings SafeZipper.empty GlobalSettings.default)
        , test "EditingGlobalSettings -> ClosingSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings SafeZipper.empty GlobalSettings.default
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings SafeZipper.empty GlobalSettings.default)
        ]


confirmAddBoard : Test
confirmAddBoard =
    describe "confirmAddBoard"
        [ test "AddingBoard -> EditingBoard focussed on the new board which is on the end" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ]) GlobalSettings.default)
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


confirmDeleteBoard : Test
confirmDeleteBoard =
    describe "confirmDeleteBoard"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "deletes the current board and -> Adding if DeletingBoard and there is ONLY one board" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default SafeZipper.empty GlobalSettings.default)
        , test "deletes the current board and -> EditingBoard if DeletingBoard and there is MORE THAN one board" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.next <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ]) GlobalSettings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.next <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigDateBoard ]) GlobalSettings.default)
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


deleteBoardRequested : Test
deleteBoardRequested =
    describe "deleteBoardRequested"
        [ test "AddingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingPlugin -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingSettings -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if already DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "EditingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "EditingGlobalSettings -> DeletingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


editBoardAt : Test
editBoardAt =
    describe "editBoardAt"
        [ test "AddingBoard -> EditingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingPlugin -> EditingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingSettings -> EditingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "EditingBoard -> EditingBoard (switched)" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "EditingGlobalSettings -> EditingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


editGlobalSettings : Test
editGlobalSettings =
    describe "editGlobalSettings"
        [ test "AddingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingPlugin -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "ClosingSettings -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "DeletingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "EditingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing to EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


globalSettings : Test
globalSettings =
    describe "globalSettings"
        [ test "returns the configs (apart from the one being added) if in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default SafeZipper.empty GlobalSettings.default
                    |> SettingsState.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "returns the configs if in ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin SafeZipper.empty GlobalSettings.default
                    |> SettingsState.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "returns the configs if in ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings SafeZipper.empty GlobalSettings.default
                    |> SettingsState.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "returns the configs if in DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard SafeZipper.empty GlobalSettings.default
                    |> SettingsState.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "returns the configs if in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard SafeZipper.empty GlobalSettings.default
                    |> SettingsState.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "returns the configs if in EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings SafeZipper.empty GlobalSettings.default
                    |> SettingsState.globalSettings
                    |> Expect.equal GlobalSettings.default
        ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard SafeZipper.empty BoardConfig.default" <|
            \() ->
                SettingsState.init SafeZipper.empty GlobalSettings.default
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default SafeZipper.empty GlobalSettings.default)
        , test "returns EditingBoard boardConfig" <|
            \() ->
                SettingsState.init exampleBoardConfigsDateBoard GlobalSettings.default
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsDateBoard GlobalSettings.default)
        ]


updateBoardBeingAdded : Test
updateBoardBeingAdded =
    describe "updateBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigTagBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


updateBoardBeingEdited : Test
updateBoardBeingEdited =
    describe "updateBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "updates the current board if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        ]


updateGlobalSettings : Test
updateGlobalSettings =
    describe "updateGlobalSettings"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateGlobalSettings (always exampleGlobalSettings)
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateGlobalSettings (always exampleGlobalSettings)
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateGlobalSettings (always exampleGlobalSettings)
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateGlobalSettings (always exampleGlobalSettings)
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateGlobalSettings (always exampleGlobalSettings)
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default)
        , test "updates the current board if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) GlobalSettings.default
                    |> SettingsState.updateGlobalSettings (always exampleGlobalSettings)
                    |> Expect.equal (SettingsState.EditingGlobalSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]) exampleGlobalSettings)
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


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskUpdateFormat = GlobalSettings.ObsidianTasks }


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
