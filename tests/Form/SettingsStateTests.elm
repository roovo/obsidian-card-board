module Form.SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import Column
import Column.Completed as CompletedColumn
import Columns
import DefaultColumnNames
import Expect
import Filter
import Form.BoardConfig exposing (BoardConfigForm)
import Form.Columns as ColumnsForm
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.Settings exposing (SettingsForm)
import Form.SettingsState as SettingsState exposing (SettingsState)
import SafeZipper
import Settings exposing (Settings)
import Test exposing (..)


suite : Test
suite =
    concat
        [ addBoardConfirmed
        , addBoardRequested
        , addColumnConfirmed
        , addColumnRequested
        , cancelCurrentState
        , deleteBoardRequested
        , deleteColumnRequested
        , deleteConfirmed
        , editBoardAt
        , editGlobalSettings
        , init
        , mapBoardBeingAdded
        , mapBoardBeingEdited
        , mapColumnBeingAdded
        , mapCurrentColumnsForm
        , mapGlobalSettings
        ]


addBoardConfirmed : Test
addBoardConfirmed =
    describe "addBoardConfirmed"
        [ test "changes to EditingBoard if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> isInEditingBoardState
                    |> Expect.equal True
        , test "add the new board to the end if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard (NewBoardForm "new board" "emptyBoard") exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.map BoardConfig.name
                    |> SafeZipper.toList
                    |> Expect.equal [ "board 1", "board 2", "board 3", "new board" ]
        , test "does nothing if AddingColumn" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> Expect.equal (SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards)
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> Expect.equal (SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if DeletingColumn" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> Expect.equal (SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards)
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardConfirmed DefaultColumnNames.default
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards)
        ]


addBoardRequested : Test
addBoardRequested =
    describe "addBoardRequested"
        [ test "does nothing if already in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "AddingColumn -> AddingBoard" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "ClosingPlugin -> AddingBoard" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "ClosingSettings -> AddingBoard" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "DeletingBoard -> AddingBoard" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "DeletingColumn -> AddingBoard" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "EditingBoard -> AddingBoard" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "EditingGlobalSettings -> AddingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        ]


addColumnConfirmed : Test
addColumnConfirmed =
    describe "addColumnConfirmed"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "switches state to EditingBoard if AddingColumn" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> isInEditingBoardState
                    |> Expect.equal True
        , test "adds the column if AddingColumn" <|
            \() ->
                SettingsState.AddingColumn (NewColumnForm "new" "Undated") exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.current
                    |> Maybe.map BoardConfig.columns
                    |> Maybe.map Columns.toList
                    |> Maybe.withDefault []
                    |> List.map Column.name
                    |> Expect.equal [ "foo", "new" ]
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> Expect.equal (SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if DeletingColumn" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> Expect.equal (SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards)
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnConfirmed
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards)
        ]


addColumnRequested : Test
addColumnRequested =
    describe "addColumnRequested"
        [ test "AddingBoard -> AddingColumn" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        , test "does nothing if already in AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        , test "ClosingPlugin -> AddingColumn" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        , test "ClosingSettings -> AddingColumn" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        , test "DeletingBoard -> AddingColumn" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        , test "DeletingColumn -> AddingColumn" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        , test "EditingBoard -> AddingColumn" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        , test "EditingGlobalSettings -> AddingColumn" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "Completed") exampleSettingsFormWithThreeBoards)
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "AddingBoard -> ClosingPlugin if the board list is empty" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsForm
                    |> SettingsState.cancelCurrentState
                    |> isInClosingPluginState
                    |> Expect.equal True
        , test "AddingBoard -> EditingBoard if the board list is NOT empty" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "AddingColumn -> EditingBoard" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "ClosingPlugin -> ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards)
        , test "ClosingSettings -> ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "DeletingColumn -> EditingBoard" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "EditingBoard -> ClosingSettings" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        , test "EditingGlobalSettings -> ClosingSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        ]


deleteBoardRequested : Test
deleteBoardRequested =
    describe "deleteBoardRequested"
        [ test "AddingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "AddingColumn -> DeletingBoard" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "ClosingPlugin -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "ClosingSettings -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if already DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "DeletingColumn -> DeletingBoard" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "EditingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "EditingGlobalSettings -> DeletingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        ]


deleteConfirmed : Test
deleteConfirmed =
    describe "deleteConfirmed"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "does nothing if AddingColumn" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> Expect.equal (SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards)
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> Expect.equal (SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards)
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        , test "changes state to AddingBoard if DeletingBoard but there are no boards" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsForm
                    |> SettingsState.deleteConfirmed
                    |> isInAddingBoardState
                    |> Expect.equal True
        , test "deletes the board if DeletingBoard and there was only the one board" <|
            \() ->
                SettingsState.DeletingBoard { exampleSettingsForm | boardConfigForms = SafeZipper.fromList [ exampleBoardConfigForm1 ] }
                    |> SettingsState.deleteConfirmed
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> Expect.equal SafeZipper.empty
        , test "changes state to AddingBoard if DeletingBoard and there was only the one board" <|
            \() ->
                SettingsState.DeletingBoard { exampleSettingsForm | boardConfigForms = SafeZipper.fromList [ exampleBoardConfigForm1 ] }
                    |> SettingsState.deleteConfirmed
                    |> isInAddingBoardState
                    |> Expect.equal True
        , test "deletes the board if DeletingBoard and there were multiple boards" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "board 1", "board 3" ]
        , test "changes state to EditingBoard if DeletingBoard and there were multiple boards" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> isInEditingBoardState
                    |> Expect.equal True
        , test "deletes the column from the current board if DeletingColumn" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.current
                    |> Maybe.map BoardConfig.columns
                    |> Maybe.map Columns.toList
                    |> Maybe.withDefault []
                    |> List.map Column.name
                    |> Expect.equal []
        , test "changes state to EditingBoard if DeletingColumn" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> isInEditingBoardState
                    |> Expect.equal True
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.deleteConfirmed
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards)
        ]


deleteColumnRequested : Test
deleteColumnRequested =
    describe "deleteColumnRequested"
        [ test "AddingBoard -> DeletingColumn" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        , test "AddingColumn -> DeletingColumn" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        , test "ClosingPlugin -> DeletingColumn" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        , test "ClosingSettings -> DeletingColumn" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        , test "DeletingBoard -> DeletingColumn" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        , test "DeletingColumn -> DeletingColumn (will update the column)" <|
            \() ->
                SettingsState.DeletingColumn 2 exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        , test "EditingBoard -> DeletingColumn" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        , test "EditingGlobalSettings -> DeletingColumn" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsForm
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 exampleSettingsForm)
        ]


editBoardAt : Test
editBoardAt =
    describe "editBoardAt"
        [ test "AddingBoard -> EditingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "AddingColumn -> EditingBoard" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "ClosingPlugin -> EditingBoard" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "ClosingSettings -> EditingBoard" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "DeletingColumn -> EditingBoard" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "EditingBoard -> EditingBoard (no change if the board isn't changed)" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.current
                    |> Maybe.map BoardConfig.name
                    |> Expect.equal (Just "board 2")
        , test "EditingBoard -> EditingBoard (changes the board)" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 2
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.current
                    |> Maybe.map BoardConfig.name
                    |> Expect.equal (Just "board 3")
        , test "EditingGlobalSettings -> EditingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        ]


editGlobalSettings : Test
editGlobalSettings =
    describe "editGlobalSettings"
        [ test "AddingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        , test "AddingColumn -> EditingGlobalSettings" <|
            \() ->
                SettingsState.AddingColumn (NewColumnForm "" "") exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        , test "ClosingPlugin -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        , test "ClosingSettings -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        , test "DeletingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        , test "DeletingColumn -> EditingGlobalSettings" <|
            \() ->
                SettingsState.DeletingColumn 1 exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        , test "EditingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        , test "does nothing to EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsForm
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsForm)
        ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard with the default NewBoardForm if there are no existing board configs" <|
            \() ->
                SettingsState.init Settings.default
                    |> newBoardFormBeingAdded
                    |> Expect.equal (Just NewBoardForm.default)
        , test "returns AddingBoard with no board configs if there are no existing board configs" <|
            \() ->
                SettingsState.init Settings.default
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> Expect.equal SafeZipper.empty
        , test "returns EditingBoard with the board configs if there are some board configs" <|
            \() ->
                SettingsState.init exampleSettingsWithBoards
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "board 1", "board 2", "board 3" ]
        ]


mapBoardBeingAdded : Test
mapBoardBeingAdded =
    describe "mapBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard (NewBoardForm "foo" "emptyBoard") exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always <| NewBoardForm "bar" "dateBoard")
                    |> Expect.equal (SettingsState.AddingBoard (NewBoardForm "bar" "dateBoard") exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always NewBoardForm.default)
                    |> Expect.equal (SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always NewBoardForm.default)
                    |> Expect.equal (SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always NewBoardForm.default)
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always NewBoardForm.default)
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always NewBoardForm.default)
                    |> Expect.equal (SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always NewBoardForm.default)
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapBoardBeingAdded (always NewBoardForm.default)
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards)
        ]


mapBoardBeingEdited : Test
mapBoardBeingEdited =
    describe "mapBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the EditingBoard state and there are no boards" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.EditingBoard exampleSettingsForm
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "updates the current board if it is in the EditingBoard state and there is a current board" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal
                        (SettingsState.EditingBoard
                            { exampleSettingsForm
                                | boardConfigForms =
                                    SafeZipper.fromList [ exampleBoardConfigForm1, exampleBoardConfigForm1, exampleBoardConfigForm3 ]
                                        |> SafeZipper.atIndex 1
                            }
                        )
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        ]


mapCurrentColumnsForm : Test
mapCurrentColumnsForm =
    describe "mapCurrentColumnsForm"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        , test "does nothing if it is in the EditingBoard state with no boards" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.EditingBoard exampleSettingsForm
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        , test "updates the columns of the current board if it is in the EditingBoard state with a current board" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.current
                    |> Maybe.map BoardConfig.columns
                    |> Expect.equal (Just Columns.empty)
        , test "does nothing if it is in the EditingGlobalSettings state with no boards" <|
            \() ->
                let
                    initialState : SettingsState
                    initialState =
                        SettingsState.EditingGlobalSettings exampleSettingsForm
                in
                initialState
                    |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> Expect.equal initialState
        ]


mapColumnBeingAdded : Test
mapColumnBeingAdded =
    describe "mapColumnBeingAdded"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always NewColumnForm.default)
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards)
        , test "maps the board being added if it is in the AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always <| NewColumnForm "bar" "Completed")
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "bar" "Completed") exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always NewColumnForm.default)
                    |> Expect.equal (SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always NewColumnForm.default)
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always NewColumnForm.default)
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always NewColumnForm.default)
                    |> Expect.equal (SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always NewColumnForm.default)
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsFormWithThreeBoards)
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                    |> SettingsState.mapColumnBeingAdded (always NewColumnForm.default)
                    |> Expect.equal (SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards)
        ]


mapGlobalSettings : Test
mapGlobalSettings =
    describe "mapGlobalSettings"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardForm.default exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default exampleSettingsForm)
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn NewColumnForm.default exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> Expect.equal (SettingsState.AddingColumn NewColumnForm.default exampleSettingsForm)
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> Expect.equal (SettingsState.ClosingPlugin exampleSettingsForm)
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> Expect.equal (SettingsState.ClosingSettings exampleSettingsForm)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> Expect.equal (SettingsState.DeletingBoard exampleSettingsForm)
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 0 exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> Expect.equal (SettingsState.DeletingColumn 0 exampleSettingsForm)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> Expect.equal (SettingsState.EditingBoard exampleSettingsForm)
        , test "applies the function if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings exampleSettingsForm
                    |> SettingsState.mapGlobalSettings (\s -> { s | future = "new future" })
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> .defaultColumnNames
                    |> DefaultColumnNames.nameFor "future"
                    |> Expect.equal "new future"
        ]



-- HELPERS


exampleBoardConfig1 : BoardConfig
exampleBoardConfig1 =
    BoardConfig.fromConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , name = "board 1"
        , showColumnTags = False
        , showFilteredTags = False
        }


exampleBoardConfig2 : BoardConfig
exampleBoardConfig2 =
    BoardConfig.fromConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , name = "board 2"
        , showColumnTags = False
        , showFilteredTags = False
        }


exampleBoardConfig3 : BoardConfig
exampleBoardConfig3 =
    BoardConfig.fromConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , name = "board 3"
        , showColumnTags = False
        , showFilteredTags = False
        }


exampleBoardConfigForm1 : BoardConfigForm
exampleBoardConfigForm1 =
    { columnsForm = ColumnsForm.init <| Columns.fromList [ Column.completed <| CompletedColumn.init "" 0 10 ]
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = "board 1"
    , showColumnTags = False
    , showFilteredTags = False
    }


exampleBoardConfigForm2 : BoardConfigForm
exampleBoardConfigForm2 =
    { columnsForm = ColumnsForm.init <| Columns.fromList [ Column.otherTags "foo" [] ]
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = "board 2"
    , showColumnTags = False
    , showFilteredTags = False
    }


exampleBoardConfigForm3 : BoardConfigForm
exampleBoardConfigForm3 =
    { columnsForm = ColumnsForm.init <| Columns.fromList [ Column.undated "bar", Column.untagged "baz" ]
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = "board 3"
    , showColumnTags = False
    , showFilteredTags = False
    }


exampleSettingsForm : SettingsForm
exampleSettingsForm =
    { boardConfigForms = SafeZipper.empty
    , completed = ""
    , filters = []
    , firstDayOfWeek = ""
    , future = ""
    , ignoreFileNameDates = False
    , otherTags = ""
    , taskCompletionFormat = ""
    , taskCompletionInLocalTime = False
    , taskCompletionShowUtcOffset = False
    , today = ""
    , tomorrow = ""
    , undated = ""
    , untagged = ""
    }


exampleSettingsFormWithThreeBoards : SettingsForm
exampleSettingsFormWithThreeBoards =
    { exampleSettingsForm
        | boardConfigForms =
            SafeZipper.fromList [ exampleBoardConfigForm1, exampleBoardConfigForm2, exampleBoardConfigForm3 ]
                |> SafeZipper.atIndex 1
    }


exampleSettingsWithBoards : Settings
exampleSettingsWithBoards =
    let
        defaultSettings : Settings
        defaultSettings =
            Settings.default
    in
    { defaultSettings | boardConfigs = SafeZipper.fromList [ exampleBoardConfig1, exampleBoardConfig2, exampleBoardConfig3 ] }


isInAddingBoardState : SettingsState -> Bool
isInAddingBoardState settingsState =
    case settingsState of
        SettingsState.AddingBoard _ _ ->
            True

        _ ->
            False


isInClosingPluginState : SettingsState -> Bool
isInClosingPluginState settingsState =
    case settingsState of
        SettingsState.ClosingPlugin _ ->
            True

        _ ->
            False


isInEditingBoardState : SettingsState -> Bool
isInEditingBoardState settingsState =
    case settingsState of
        SettingsState.EditingBoard _ ->
            True

        _ ->
            False


newBoardFormBeingAdded : SettingsState -> Maybe NewBoardForm
newBoardFormBeingAdded settingsState =
    case settingsState of
        SettingsState.AddingBoard newBoardForm _ ->
            Just newBoardForm

        _ ->
            Nothing
