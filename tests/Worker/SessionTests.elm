module Worker.SessionTests exposing (suite)

-- import BoardConfig
-- import Card
-- import Column
-- import Column.Completed as CompletedColumn
-- import Columns
-- import DragAndDrop.DragData as DragData
-- import DragAndDrop.DragTracker as DragTracker
-- import Filter
-- import GlobalSettings exposing (GlobalSettings)
-- import Helpers.FilterHelpers as FilterHelpers
-- import Helpers.TaskItemHelpers as TaskItemHelpers
-- import Parser
-- import SafeZipper

import DataviewTaskCompletion
import Expect
import Helpers.TaskListHelpers as TaskListHelpers
import TaskItem
import TaskList
import Test exposing (..)
import Worker.InteropDefinitions exposing (Flags)
import Worker.Session as Session



-- import Time


suite : Test
suite =
    concat
        [ addTaskList
        , default
        , finishAdding
        , fromFlags
        , removeTaskItems
        , replaceTaskList
        ]


addTaskList : Test
addTaskList =
    describe "addTaskList"
        [ test "can add a tasklist to a new session" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2" ]
        , test "can add tasklists onto a session which already contains one" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2", "g1", "g2" ]
        , test "can add tasklist even if previously finished adding" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.finishAdding
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2", "g1", "g2" ]
        ]


default : Test
default =
    describe "default"
        [ test "has an empty task list" <|
            \() ->
                Session.default
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "has DataviewTaskCompletion.Text completion" <|
            \() ->
                Session.default
                    |> Session.dataviewTaskCompletion
                    |> Expect.equal (DataviewTaskCompletion.Text "completion")
        ]


finishAdding : Test
finishAdding =
    describe "finishAdding"
        [ test "results in an empty tasklist if none have been added to the default Session" <|
            \() ->
                Session.default
                    |> Session.finishAdding
                    |> Session.taskList
                    |> Expect.equal TaskList.empty
        , test "if Loading, there are the loaded tasks in the list" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.finishAdding
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2" ]
        , test "if Loaded, stays Loaded" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.finishAdding
                    |> Session.finishAdding
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2" ]
        ]


fromFlags : Test
fromFlags =
    describe "fromFlags"
        [ test "copies the dataviewTaskCompletion value from the flags" <|
            \() ->
                { exampleFlags | dataviewTaskCompletion = DataviewTaskCompletion.Emoji }
                    |> Session.fromFlags
                    |> Session.dataviewTaskCompletion
                    |> Expect.equal DataviewTaskCompletion.Emoji
        ]


removeTaskItems : Test
removeTaskItems =
    describe "removeTaskItems"
        [ test "does nothing if there are no taskItems in the Session" <|
            \() ->
                Session.default
                    |> Session.removeTaskItems []
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "removes tasks with the given ids whilst loading tasklists" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.removeTaskItems [ "3826002220:2", "3792446982:3" ]
                    |> Session.taskList
                    |> TaskList.toList
                    |> List.map TaskItem.id
                    |> Expect.equal [ "3826002220:3", "3792446982:2" ]
        , test "removes tasks with the given ids if finished loading tasklists" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.finishAdding
                    |> Session.removeTaskItems [ "3826002220:2", "3792446982:3" ]
                    |> Session.taskList
                    |> TaskList.toList
                    |> List.map TaskItem.id
                    |> Expect.equal [ "3826002220:3", "3792446982:2" ]
        ]


replaceTaskList : Test
replaceTaskList =
    describe "replaceTaskList"
        [ test "does nothing if a new session (state -> Waiting)" <|
            \() ->
                Session.default
                    |> Session.replaceTaskList TaskListHelpers.taskListFromFileA
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "replaces all tasks with those given whilst loading tasklists" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.replaceTaskList (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "n1", "n2" ]
        , test "replaces all tasks with those given whilst loading tasklists even if finished adding" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.finishAdding
                    |> Session.replaceTaskList (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "n1", "n2" ]
        ]



-- -- HELPERS


exampleFlags : Flags
exampleFlags =
    { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
    }
