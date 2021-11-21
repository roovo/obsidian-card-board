module ModelTests exposing (suite)

import Expect
import Helpers.TaskListHelpers as TaskListHelpers
import Model
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
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loading, adds the tasks to those already loaded" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.addTaskList TaskListHelpers.taskListFromFileG
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2", "g1", "g2" ])
        , test "if Loaded, adds the tasks to those already loaded" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.finishAdding
                    |> Model.addTaskList TaskListHelpers.taskListFromFileG
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2", "g1", "g2" ])
        ]


default : Test
default =
    describe "default"
        [ test "sets the tasklist state to waiting" <|
            \() ->
                Model.default
                    |> .taskList
                    |> Expect.equal State.Waiting
        ]


deleteItemsFromFile : Test
deleteItemsFromFile =
    describe "deleteItemsFromFile"
        [ test "if Waiting, leaves as Waiting" <|
            \() ->
                Model.default
                    |> Model.deleteItemsFromFile ""
                    |> .taskList
                    |> Expect.equal State.Waiting
        , test "if Loading, removes tasks from the file from the Loading list" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.addTaskList TaskListHelpers.taskListFromFileG
                    |> Model.deleteItemsFromFile "g"
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loaded, removes tasks from the file from the Loaded list" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.addTaskList TaskListHelpers.taskListFromFileG
                    |> Model.finishAdding
                    |> Model.deleteItemsFromFile "g"
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2" ])
        ]


finishAdding : Test
finishAdding =
    describe "finishAdding"
        [ test "if Waiting, sets to be Loaded with no tasks in the list" <|
            \() ->
                Model.default
                    |> Model.finishAdding
                    |> .taskList
                    |> Expect.equal (State.Loaded TaskList.empty)
        , test "if Loading, sets to be Loaded with the loaded tasks in the list" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.finishAdding
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2" ])
        , test "if Loaded, stays Loaded" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.finishAdding
                    |> Model.finishAdding
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2" ])
        ]


updateTaskItems : Test
updateTaskItems =
    describe "updateTaskItems"
        [ test "if Waiting, sets to Loading with the given tasks" <|
            \() ->
                Model.default
                    |> Model.updateTaskItems "" TaskListHelpers.taskListFromFileA
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loading, replaces tasks from the file with those given" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.addTaskList TaskListHelpers.taskListFromFileG
                    |> Model.updateTaskItems "a" (TaskListHelpers.taskListFromNewFile "path")
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "n1", "n2", "g1", "g2" ])
        , test "if Loaded, replaces tasks from the file with those given" <|
            \() ->
                Model.default
                    |> Model.addTaskList TaskListHelpers.taskListFromFileA
                    |> Model.addTaskList TaskListHelpers.taskListFromFileG
                    |> Model.finishAdding
                    |> Model.updateTaskItems "g" (TaskListHelpers.taskListFromNewFile "path")
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "n1", "n2", "a1", "a2" ])
        ]
