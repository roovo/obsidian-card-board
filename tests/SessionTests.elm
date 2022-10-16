module SessionTests exposing (suite)

import BoardConfig
import CardBoardSettings
import Expect
import Filter
import GlobalSettings
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import SafeZipper
import Session
import State
import TaskItem
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ addTaskList
        , default
        , deleteItemsFromFile
        , finishAdding
        , globalSettings
        , replaceTaskItems
        , updatePath
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


globalSettings : Test
globalSettings =
    describe "globalSettings"
        [ test "if Waiting, sets to be Loaded with no tasks in the list" <|
            \() ->
                Session.default
                    |> Session.globalSettings
                    |> Expect.equal GlobalSettings.default
        ]


updatePath : Test
updatePath =
    describe "updatePath"
        [ test "updates taskList paths" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.updatePath "a" "b"
                    |> Session.taskList
                    |> State.map (\tl -> TaskList.foldl (\i acc -> TaskItem.filePath i :: acc) [] tl)
                    |> Expect.equal (State.Loading [ "b", "b" ])
        , test "updates filter paths" <|
            \() ->
                Session.default
                    |> Session.mapConfig (\c -> { c | boardConfigs = SafeZipper.fromList [ BoardConfig.DateBoardConfig BoardConfigHelpers.exampleDateBoardConfig ] })
                    |> Session.updatePath "a/path" "a"
                    |> Session.updatePath "b/path" "b"
                    |> Session.boardConfigs
                    |> SafeZipper.toList
                    |> List.concatMap BoardConfig.filters
                    |> List.map Filter.value
                    |> Expect.equal [ "a", "b", "tag1", "tag2" ]
        ]


replaceTaskItems : Test
replaceTaskItems =
    describe "replaceTaskItems"
        [ test "if Waiting, leave as Waiting" <|
            \() ->
                Session.default
                    |> Session.replaceTaskItems "" TaskListHelpers.taskListFromFileA
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal State.Waiting
        , test "if Loading, replaces tasks from the file with those given" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.replaceTaskItems "a" (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loading [ "n1", "n2", "g1", "g2" ])
        , test "if Loaded, replaces tasks from the file with those given" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.finishAdding
                    |> Session.replaceTaskItems "g" (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> State.map TaskList.taskTitles
                    |> Expect.equal (State.Loaded [ "n1", "n2", "a1", "a2" ])
        ]



-- HELPERS
