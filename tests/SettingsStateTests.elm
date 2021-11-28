module SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import DateBoard
import Expect
import Filter
import SafeZipper exposing (SafeZipper)
import SettingsState
import TagBoard
import Test exposing (..)


suite : Test
suite =
    concat
        [ forNoConfig
        , startEditing
        , mapBoardBeingAdded
        , mapBoardBeingEdited
        , moveToAdd
        , confirmAdd
        , confirmDelete
        , moveToDelete
        , changeBoardBeingEdited
        , cancelCurrentState
        ]


forNoConfig : Test
forNoConfig =
    describe "forNoConfig"
        [ test "returns AddingBoard SafeZipper.empty BoardConfig.default" <|
            \() ->
                SettingsState.forNoConfig
                    |> Expect.equal (SettingsState.AddingBoard SafeZipper.empty BoardConfig.default)
        ]


startEditing : Test
startEditing =
    describe "startEditing"
        [ test "returns EditingBoard boardConfig" <|
            \() ->
                exampleBoardConfigsDateBoard
                    |> SettingsState.startEditing
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsDateBoard)
        ]


mapBoardBeingAdded : Test
mapBoardBeingAdded =
    describe "mapBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard exampleBoardConfigsDateBoard)
        ]


mapBoardBeingEdited : Test
mapBoardBeingEdited =
    describe "mapBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard)
        , test "maps the current board if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsTagBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.DeletingBoard exampleBoardConfigsDateBoard)
        ]


moveToAdd : Test
moveToAdd =
    describe "moveToAdd"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsState.moveToAdd
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard)
        , test "moves to the AddingBoard state if it is in Editing state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.moveToAdd
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigsDateBoard BoardConfig.default)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.moveToAdd
                    |> Expect.equal (SettingsState.DeletingBoard exampleBoardConfigsDateBoard)
        ]


confirmAdd : Test
confirmAdd =
    describe "confirmAdd"
        [ test "adds the config and moves to EditingBoard (focussed on the last board) if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard
                    |> SettingsState.confirmAdd
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last exampleBoardConfigsBoth))
        , test "does nothing if it is in Editing state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.confirmAdd
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.confirmAdd
                    |> Expect.equal (SettingsState.DeletingBoard exampleBoardConfigsDateBoard)
        ]


moveToDelete : Test
moveToDelete =
    describe "moveToDelete"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsState.moveToDelete
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard)
        , test "moves to the DeletingBoard state if it is in Editing state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.moveToDelete
                    |> Expect.equal (SettingsState.DeletingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.moveToDelete
                    |> Expect.equal (SettingsState.DeletingBoard exampleBoardConfigsDateBoard)
        ]


confirmDelete : Test
confirmDelete =
    describe "confirmDelete"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard)
        , test "does nothing if it is in Editing state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsDateBoard)
        , test "sets it to Adding a default board if it is in the DeletingBoard state and is the last board" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.AddingBoard SafeZipper.empty BoardConfig.default)
        , test "sets it to Editing with the board deleted if it is in the DeletingBoard state and is NOT the last board" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsBoth
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.EditingBoard exampleBoardConfigsTagBoard)
        ]


changeBoardBeingEdited : Test
changeBoardBeingEdited =
    describe "changeBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsBoth exampleBoardConfigTagBoard
                    |> SettingsState.changeBoardBeingEdited 1
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigsBoth exampleBoardConfigTagBoard)
        , test "changes to the board at the given index if it is in Editing state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsBoth
                    |> SettingsState.changeBoardBeingEdited 1
                    |> Expect.equal (SettingsState.EditingBoard (SafeZipper.last exampleBoardConfigsBoth))
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsBoth
                    |> SettingsState.changeBoardBeingEdited 1
                    |> Expect.equal (SettingsState.DeletingBoard exampleBoardConfigsBoth)
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "says to change to Editing state if it is in the AddingBoard state and there are some configs" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigsBoth exampleBoardConfigTagBoard
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsState.EditingBoard exampleBoardConfigsBoth
                        , SettingsState.SetToState
                        )
        , test "says to exit with no config if it is in the AddingBoard state and there are no configs" <|
            \() ->
                SettingsState.AddingBoard SafeZipper.empty exampleBoardConfigTagBoard
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsState.EditingBoard SafeZipper.empty
                        , SettingsState.ExitWithNoConfig
                        )
        , test "says to exit with the current config it is in Editing state" <|
            \() ->
                SettingsState.EditingBoard exampleBoardConfigsBoth
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsState.EditingBoard exampleBoardConfigsBoth
                        , SettingsState.ExitWithConfig exampleBoardConfigsBoth
                        )
        , test "says to change to EditingBoard state if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard exampleBoardConfigsBoth
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsState.EditingBoard exampleBoardConfigsBoth
                        , SettingsState.SetToState
                        )
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


exampleBoardConfigsTagBoard : SafeZipper BoardConfig
exampleBoardConfigsTagBoard =
    SafeZipper.fromList <| [ exampleBoardConfigTagBoard ]


exampleBoardConfigsBoth : SafeZipper BoardConfig
exampleBoardConfigsBoth =
    SafeZipper.fromList <| [ exampleBoardConfigDateBoard, exampleBoardConfigTagBoard ]


exampleDateBoardConfig : DateBoard.Config
exampleDateBoardConfig =
    { completedCount = 12
    , filters = []
    , includeUndated = False
    , title = "Date Board Title"
    }


exampleTagBoardConfig : TagBoard.Config
exampleTagBoardConfig =
    { columns = [ { tag = "foo", displayTitle = "bar" } ]
    , completedCount = 6
    , filters = [ Filter.PathFilter "a", Filter.PathFilter "b", Filter.TagFilter "t1", Filter.TagFilter "t2" ]
    , includeOthers = False
    , includeUntagged = True
    , title = "Tag Board Title"
    }
