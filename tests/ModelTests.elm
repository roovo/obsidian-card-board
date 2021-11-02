module ModelTests exposing (suite)

import Expect
import Model
import State
import TaskList
import TaskListTestHelpers exposing (parsedTasks, tasksFromFileA, tasksFromFileG, tasksFromNewFile)
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
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loading, adds the tasks to those already loaded" <|
            \() ->
                Model.default
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> Model.addTaskList (parsedTasks tasksFromFileG)
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2", "g1", "g2" ])
        , test "if Loaded, adds the tasks to those already loaded" <|
            \() ->
                Model.default
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> Model.finishAdding
                    |> Model.addTaskList (parsedTasks tasksFromFileG)
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
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> Model.addTaskList (parsedTasks tasksFromFileG)
                    |> Model.deleteItemsFromFile "g"
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loaded, removes tasks from the file from the Loaded list" <|
            \() ->
                Model.default
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> Model.addTaskList (parsedTasks tasksFromFileG)
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
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> Model.finishAdding
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "a1", "a2" ])
        , test "if Loaded, stays Loaded" <|
            \() ->
                Model.default
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
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
                    |> Model.updateTaskItems "" (parsedTasks tasksFromFileA)
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "a1", "a2" ])
        , test "if Loading, replaces tasks from the file with those given" <|
            \() ->
                Model.default
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> Model.addTaskList (parsedTasks tasksFromFileG)
                    |> Model.updateTaskItems "a" (parsedTasks <| tasksFromNewFile "path")
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "n1", "n2", "g1", "g2" ])
        , test "if Loaded, replaces tasks from the file with those given" <|
            \() ->
                Model.default
                    |> Model.addTaskList (parsedTasks tasksFromFileA)
                    |> Model.addTaskList (parsedTasks tasksFromFileG)
                    |> Model.finishAdding
                    |> Model.updateTaskItems "g" (parsedTasks <| tasksFromNewFile "path")
                    |> .taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "n1", "n2", "a1", "a2" ])
        ]
