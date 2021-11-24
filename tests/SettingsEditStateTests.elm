module SettingsEditStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import DateBoard
import Expect
import Filter
import SafeZipper exposing (SafeZipper)
import SettingsEditState
import TagBoard
import Test exposing (..)


suite : Test
suite =
    concat
        [ default
        , forNoConfig
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


default : Test
default =
    describe "default"
        [ test "returns NotBeingEdited" <|
            \() ->
                SettingsEditState.default
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


forNoConfig : Test
forNoConfig =
    describe "forNoConfig"
        [ test "returns AddingBoard SafeZipper.empty BoardConfig.default" <|
            \() ->
                SettingsEditState.forNoConfig
                    |> Expect.equal (SettingsEditState.AddingBoard SafeZipper.empty BoardConfig.default)
        ]


startEditing : Test
startEditing =
    describe "startEditing"
        [ test "returns EditingBoard boardConfig" <|
            \() ->
                exampleBoardConfigsDateBoard
                    |> SettingsEditState.startEditing
                    |> Expect.equal (SettingsEditState.EditingBoard exampleBoardConfigsDateBoard)
        ]


mapBoardBeingAdded : Test
mapBoardBeingAdded =
    describe "mapBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsEditState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsEditState.EditingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


mapBoardBeingEdited : Test
mapBoardBeingEdited =
    describe "mapBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsEditState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard)
        , test "maps the current board if it is in the EditingBoard state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsEditState.EditingBoard exampleBoardConfigsTagBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.mapBoardBeingEdited (always exampleBoardConfigTagBoard)
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


moveToAdd : Test
moveToAdd =
    describe "moveToAdd"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsEditState.moveToAdd
                    |> Expect.equal (SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard)
        , test "moves to the AddingBoard state if it is in Editing state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.moveToAdd
                    |> Expect.equal (SettingsEditState.AddingBoard exampleBoardConfigsDateBoard BoardConfig.default)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.moveToAdd
                    |> Expect.equal (SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.moveToAdd
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


confirmAdd : Test
confirmAdd =
    describe "confirmAdd"
        [ test "adds the config and moves to EditingBoard (focussed on the last board) if it is in the AddingBoard state" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard
                    |> SettingsEditState.confirmAdd
                    |> Expect.equal (SettingsEditState.EditingBoard (SafeZipper.last exampleBoardConfigsBoth))
        , test "does nothing if it is in Editing state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.confirmAdd
                    |> Expect.equal (SettingsEditState.EditingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.confirmAdd
                    |> Expect.equal (SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.confirmAdd
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


moveToDelete : Test
moveToDelete =
    describe "moveToDelete"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard
                    |> SettingsEditState.moveToDelete
                    |> Expect.equal (SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigDateBoard)
        , test "moves to the DeletingBoard state if it is in Editing state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.moveToDelete
                    |> Expect.equal (SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.moveToDelete
                    |> Expect.equal (SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard)
        , test "does nothing if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.moveToDelete
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


confirmDelete : Test
confirmDelete =
    describe "confirmDelete"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard
                    |> SettingsEditState.confirmDelete
                    |> Expect.equal (SettingsEditState.AddingBoard exampleBoardConfigsDateBoard exampleBoardConfigTagBoard)
        , test "does nothing if it is in Editing state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.confirmDelete
                    |> Expect.equal (SettingsEditState.EditingBoard exampleBoardConfigsDateBoard)
        , test "sets it to Adding a default board if it is in the DeletingBoard state and is the last board" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsDateBoard
                    |> SettingsEditState.confirmDelete
                    |> Expect.equal (SettingsEditState.AddingBoard SafeZipper.empty BoardConfig.default)
        , test "sets it to Editing with the board deleted if it is in the DeletingBoard state and is NOT the last board" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsBoth
                    |> SettingsEditState.confirmDelete
                    |> Expect.equal (SettingsEditState.EditingBoard exampleBoardConfigsTagBoard)
        , test "does nothing if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.confirmDelete
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


changeBoardBeingEdited : Test
changeBoardBeingEdited =
    describe "changeBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsBoth exampleBoardConfigTagBoard
                    |> SettingsEditState.changeBoardBeingEdited 1
                    |> Expect.equal (SettingsEditState.AddingBoard exampleBoardConfigsBoth exampleBoardConfigTagBoard)
        , test "changes to the board at the given index if it is in Editing state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsBoth
                    |> SettingsEditState.changeBoardBeingEdited 1
                    |> Expect.equal (SettingsEditState.EditingBoard (SafeZipper.last exampleBoardConfigsBoth))
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsBoth
                    |> SettingsEditState.changeBoardBeingEdited 1
                    |> Expect.equal (SettingsEditState.DeletingBoard exampleBoardConfigsBoth)
        , test "does nothing if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.changeBoardBeingEdited 1
                    |> Expect.equal SettingsEditState.NotBeingEdited
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "says to change to Editing state if it is in the AddingBoard state and there are some configs" <|
            \() ->
                SettingsEditState.AddingBoard exampleBoardConfigsBoth exampleBoardConfigTagBoard
                    |> SettingsEditState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsEditState.EditingBoard exampleBoardConfigsBoth
                        , SettingsEditState.SetToState
                        )
        , test "says to exit with no config if it is in the AddingBoard state and there are no configs" <|
            \() ->
                SettingsEditState.AddingBoard SafeZipper.empty exampleBoardConfigTagBoard
                    |> SettingsEditState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsEditState.EditingBoard SafeZipper.empty
                        , SettingsEditState.ExitWithNoConfig
                        )
        , test "says to exit with the current config it is in Editing state" <|
            \() ->
                SettingsEditState.EditingBoard exampleBoardConfigsBoth
                    |> SettingsEditState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsEditState.NotBeingEdited
                        , SettingsEditState.ExitWithConfig exampleBoardConfigsBoth
                        )
        , test "says to change to EditingBoard state if it is in the DeletingBoard state" <|
            \() ->
                SettingsEditState.DeletingBoard exampleBoardConfigsBoth
                    |> SettingsEditState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsEditState.EditingBoard exampleBoardConfigsBoth
                        , SettingsEditState.SetToState
                        )
        , test "says to keep in the same state if it is in the NotBeingEdited state" <|
            \() ->
                SettingsEditState.NotBeingEdited
                    |> SettingsEditState.cancelCurrentState
                    |> Expect.equal
                        ( SettingsEditState.NotBeingEdited
                        , SettingsEditState.SetToState
                        )
        ]



-- , cancelCurrentState
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
