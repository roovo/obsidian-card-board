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
        [ init
        , mapBoardBeingAdded
        , addState
        , confirmAdd
        , confirmDelete
        , deleteState
        , cancelCurrentState
        ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard SafeZipper.empty BoardConfig.default" <|
            \() ->
                SafeZipper.empty
                    |> SettingsState.init
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default)
        , test "returns EditingBoard boardConfig" <|
            \() ->
                exampleBoardConfigsDateBoard
                    |> SettingsState.init
                    |> Expect.equal SettingsState.EditingBoard
        ]


mapBoardBeingAdded : Test
mapBoardBeingAdded =
    describe "mapBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SafeZipper.empty
                    |> SettingsState.init
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal (SettingsState.AddingBoard exampleBoardConfigTagBoard)
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal SettingsState.EditingBoard
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard
                    |> SettingsState.mapBoardBeingAdded (always exampleBoardConfigTagBoard)
                    |> Expect.equal SettingsState.DeletingBoard
        ]


addState : Test
addState =
    describe "addState"
        [ test "returns the AddingBoard state with a default config" <|
            \() ->
                SettingsState.addState
                    |> Expect.equal (SettingsState.AddingBoard BoardConfig.default)
        ]


confirmAdd : Test
confirmAdd =
    describe "confirmAdd"
        [ test "adds the config and moves to EditingBoard (focussed on the last board) if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigTagBoard
                    |> SettingsState.confirmAdd
                    |> Expect.equal (SettingsState.AddBoard exampleBoardConfigTagBoard SettingsState.EditingBoard)
        , test "does nothing if it is in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard
                    |> SettingsState.confirmAdd
                    |> Expect.equal SettingsState.NoAction
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard
                    |> SettingsState.confirmAdd
                    |> Expect.equal SettingsState.NoAction
        ]


deleteState : Test
deleteState =
    describe "deleteState"
        [ test "returns the DeletingBoard state" <|
            \() ->
                SettingsState.deleteState
                    |> Expect.equal SettingsState.DeletingBoard
        ]


confirmDelete : Test
confirmDelete =
    describe "confirmDelete"
        [ test "returns NoAction if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigTagBoard
                    |> SettingsState.confirmDelete
                    |> Expect.equal SettingsState.NoAction
        , test "returns NoAction if it is in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard
                    |> SettingsState.confirmDelete
                    |> Expect.equal SettingsState.NoAction
        , test "returns DeleteCurrent if in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard
                    |> SettingsState.confirmDelete
                    |> Expect.equal SettingsState.DeleteCurrent
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "says to change to AddCancelled state if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard exampleBoardConfigTagBoard
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.AddCancelled SettingsState.EditingBoard)
        , test "says to change to EditingBoard state if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.SetToState SettingsState.EditingBoard)
        , test "says to exit if it is in Editing state" <|
            \() ->
                SettingsState.EditingBoard
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal SettingsState.Exit
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
