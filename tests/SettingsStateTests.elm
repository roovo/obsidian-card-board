module SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
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
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "ClosingPlugin -> AddingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "ClosingSettings -> AddingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "DeletingBoard -> AddingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "EditingBoard -> AddingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        ]


boardConfigs : Test
boardConfigs =
    describe "boardConfigs"
        [ test "returns the configs (apart from the one being added) if in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        , test "returns the configs if in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "AddingBoard -> ClosingPlugin if the board list is empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default SafeZipper.empty
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin SafeZipper.empty)
        , test "AddingBoard -> EditingBoard if the board list is NOT empty" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "ClosingPlugin -> ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin SafeZipper.empty
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin SafeZipper.empty)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard SafeZipper.empty
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard SafeZipper.empty)
        , test "EditingBoard -> ClosingSettings" <|
            \() ->
                SettingsState.EditingBoard SafeZipper.empty
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings SafeZipper.empty)
        , test "ClosingSettings -> ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings SafeZipper.empty
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings SafeZipper.empty)
        ]


confirmAddBoard : Test
confirmAddBoard =
    describe "confirmAddBoard"
        [ test "AddingBoard -> EditingBoard focussed on the new board which is on the end" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ]))
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmAddBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        ]


confirmDeleteBoard : Test
confirmDeleteBoard =
    describe "confirmDeleteBoard"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigDateBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "deletes the current board and -> Adding if DeletingBoard and there is ONLY one board" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default SafeZipper.empty)
        , test "deletes the current board and -> EditingBoard if DeletingBoard and there is MORE THAN one board" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.next <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard, exampleBoardConfigDateBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.next <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigDateBoard ]))
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.confirmDeleteBoard
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        ]


deleteBoardRequested : Test
deleteBoardRequested =
    describe "deleteBoardRequested"
        [ test "AddingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "ClosingPlugin -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "ClosingSettings -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "does nothing if already DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        , test "EditingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ])
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard ]))
        ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard SafeZipper.empty BoardConfig.default" <|
            \() ->
                SafeZipper.empty
                    |> SettingsState.init
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default SafeZipper.empty)
        , test "returns EditingBoard boardConfig" <|
            \() ->
                exampleBoardConfigsDateBoard
                    |> SettingsState.init
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsDateBoard)
        ]


switchBoardBeingEdited : Test
switchBoardBeingEdited =
    describe "switchBoardBeingEdited"
        [ test "AddingBoard -> EditingBoard" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "ClosingPlugin -> EditingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "ClosingSettings -> EditingBoard" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "EditingBoard -> EditingBoard (switched)" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.switchBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last <| SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        ]


updateBoardBeingAdded : Test
updateBoardBeingAdded =
    describe "updateBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigTagBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        ]


updateBoardBeingEdited : Test
updateBoardBeingEdited =
    describe "updateBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingPlugin (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.ClosingSettings (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]))
        , test "updates the current board if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ])
                    |> SettingsState.updateBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.fromList [ exampleBoardConfigTagBoard, exampleBoardConfigTagBoard ]))
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
