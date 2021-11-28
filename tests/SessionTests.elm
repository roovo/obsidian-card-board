module SessionTests exposing (suite)

import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import SafeZipper
import Session exposing (Session)
import SettingsState
import State
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ addTaskList
        , default
        , deleteItemsFromFile
        , finishAdding
        , updateTaskItems
        ]


addTaskList : Test
addTaskList =
    describe "addTaskList"
        [ test "if Waiting, sets to be Loading the added tasks" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loading, adds the tasks to those already loaded" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2", "g1", "g2" ])
        , test "if Loaded, adds the tasks to those already loaded" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.finishAdding
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2", "g1", "g2" ])
        ]


default : Test
default =
    describe "default"
        [ test "sets the tasklist state to waiting" <|
            \() ->
                Session.default
                    |> Session.taskList
                    |> Expect.equal State.Waiting
        ]


deleteItemsFromFile : Test
deleteItemsFromFile =
    describe "deleteItemsFromFile"
        [ test "if Waiting, leaves as Waiting" <|
            \() ->
                Session.default
                    |> Session.deleteItemsFromFile ""
                    |> Session.taskList
                    |> Expect.equal State.Waiting
        , test "if Loading, removes tasks from the file from the Loading list" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.deleteItemsFromFile "g"
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loaded, removes tasks from the file from the Loaded list" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.finishAdding
                    |> Session.deleteItemsFromFile "g"
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2" ])
        ]


finishAdding : Test
finishAdding =
    describe "finishAdding"
        [ test "if Waiting, sets to be Loaded with no tasks in the list" <|
            \() ->
                Session.default
                    |> Session.finishAdding
                    |> Session.taskList
                    |> Expect.equal (State.Loaded TaskList.empty)
        , test "if Loading, sets to be Loaded with the loaded tasks in the list" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.finishAdding
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2" ])
        , test "if Loaded, stays Loaded" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.finishAdding
                    |> Session.finishAdding
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2" ])
        ]


updateTaskItems : Test
updateTaskItems =
    describe "updateTaskItems"
        [ test "if Waiting, sets to Loading with the given tasks" <|
            \() ->
                Session.default
                    |> Session.updateTaskItems "" TaskListHelpers.taskListFromFileA
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loading, replaces tasks from the file with those given" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.updateTaskItems "a" (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "n1", "n2", "g1", "g2" ])
        , test "if Loaded, replaces tasks from the file with those given" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.finishAdding
                    |> Session.updateTaskItems "g" (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "n1", "n2", "a1", "a2" ])
        ]



-- HELPERS


defaultSession : Session
defaultSession =
    Session.default
