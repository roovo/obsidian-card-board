module SessionTests exposing (suite)

import BoardConfig
import DataviewTaskCompletion
import DragAndDrop.DragData as DragData
import DragAndDrop.DragTracker as DragTracker
import Expect
import Filter
import GlobalSettings
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import SafeZipper
import Session
import Settings exposing (Settings)
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
        , moveDragable
        , replaceTaskItems
        , stopTrackingDragable
        , updatePath
        , waitForDrag
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
        , test "is not the active view" <|
            \() ->
                Session.default
                    |> Session.isActiveView
                    |> Expect.equal False
        , test "isnt dragging" <|
            \() ->
                Session.default
                    |> Session.isDragging
                    |> Expect.equal False
        , test "has default Settings" <|
            \() ->
                Session.default
                    |> Session.settings
                    |> Expect.equal Settings.default
        , test "has DataviewTaskCompletion.Text completion" <|
            \() ->
                Session.default
                    |> Session.dataviewTaskCompletion
                    |> Expect.equal (DataviewTaskCompletion.Text "completion")
        ]


deleteItemsFromFile : Test
deleteItemsFromFile =
    describe "deleteItemsFromFile"
        [ test "does nothing to the tasklist of a new session" <|
            \() ->
                Session.default
                    |> Session.deleteItemsFromFile ""
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "remove tasks from the given file during loading" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.deleteItemsFromFile "g"
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2" ]
        , test "remove tasks from the given file after loading has finished" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.finishAdding
                    |> Session.deleteItemsFromFile "g"
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2" ]
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


globalSettings : Test
globalSettings =
    describe "globalSettings"
        [ test "set to their default in a default Session" <|
            \() ->
                Session.default
                    |> Session.globalSettings
                    |> Expect.equal GlobalSettings.default
        ]


waitForDrag : Test
waitForDrag =
    describe "waitForDrag"
        [ test "puts the dragTracker in Waiting state" <|
            \() ->
                let
                    clientData : DragTracker.ClientData
                    clientData =
                        { uniqueId = "an id"
                        , clientPos = { x = 0, y = 1 }
                        , offsetPos = { x = 1, y = 2 }
                        }
                in
                Session.default
                    |> Session.waitForDrag clientData
                    |> Session.dragTracker
                    |> Expect.equal (DragTracker.Waiting clientData)
        ]


moveDragable : Test
moveDragable =
    describe "moveDragable"
        [ test "puts the dragTracker in Dragging state" <|
            \() ->
                let
                    dragData : DragData.DragData
                    dragData =
                        { dragType = "aDragType"
                        , dragAction = DragData.Move
                        , cursor = { x = 1.1, y = 2.2 }
                        , offset = { x = 1, y = 2 }
                        , draggedNodeRect = { x = 2, y = 3, width = 4, height = 5 }
                        , beacons = []
                        }

                    clientData : DragTracker.ClientData
                    clientData =
                        { uniqueId = "an id"
                        , clientPos = { x = 0, y = 1 }
                        , offsetPos = { x = 1, y = 2 }
                        }
                in
                Session.default
                    |> Session.waitForDrag clientData
                    |> Session.moveDragable dragData
                    |> Session.dragTracker
                    |> Expect.equal
                        (DragTracker.Dragging
                            { uniqueId = "an id"
                            , clientPos = { x = 1.1, y = 2.2 }
                            , offsetPos = { x = 1, y = 2 }
                            }
                            { offset = { x = 1, y = 2 }
                            , draggedNodeStartRect = { x = 2, y = 3, width = 4, height = 5 }
                            , dragType = "aDragType"
                            }
                        )
        ]


replaceTaskItems : Test
replaceTaskItems =
    describe "replaceTaskItems"
        [ test "does nothing on a new session" <|
            \() ->
                Session.default
                    |> Session.replaceTaskItems "" TaskListHelpers.taskListFromFileA
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "replaces tasks from the file with those given whilst loading tasklists" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.replaceTaskItems "a" (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "n1", "n2", "g1", "g2" ]
        , test "replaces tasks from the file with those given whilst loading tasklists even if finished adding" <|
            \() ->
                Session.default
                    |> Session.addTaskList TaskListHelpers.taskListFromFileA
                    |> Session.addTaskList TaskListHelpers.taskListFromFileG
                    |> Session.finishAdding
                    |> Session.replaceTaskItems "g" (TaskListHelpers.taskListFromNewFile "path")
                    |> Session.taskList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "n1", "n2", "a1", "a2" ]
        ]


stopTrackingDragable : Test
stopTrackingDragable =
    describe "stopTracking"
        [ test "puts the dragTracker in Dragging state" <|
            \() ->
                let
                    dragData : DragData.DragData
                    dragData =
                        { dragType = "aDragType"
                        , dragAction = DragData.Move
                        , cursor = { x = 1.1, y = 2.2 }
                        , offset = { x = 1, y = 2 }
                        , draggedNodeRect = { x = 2, y = 3, width = 4, height = 5 }
                        , beacons = []
                        }

                    clientData : DragTracker.ClientData
                    clientData =
                        { uniqueId = "an id"
                        , clientPos = { x = 0, y = 1 }
                        , offsetPos = { x = 1, y = 2 }
                        }
                in
                Session.default
                    |> Session.waitForDrag clientData
                    |> Session.moveDragable dragData
                    |> Session.stopTrackingDragable
                    |> Session.dragTracker
                    |> Expect.equal DragTracker.NotDragging
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
                    |> TaskList.foldl (\i acc -> TaskItem.filePath i :: acc) []
                    |> Expect.equal [ "b", "b" ]
        , test "updates filter paths" <|
            \() ->
                Session.default
                    |> Session.updateSettings exampleSettings
                    |> Session.updatePath "a/path" "a"
                    |> Session.updatePath "b/path" "b"
                    |> Session.boardConfigs
                    |> SafeZipper.toList
                    |> List.concatMap BoardConfig.filters
                    |> List.map Filter.value
                    |> Expect.equal [ "a", "b", "tag1", "tag2" ]
        ]



-- HELPERS


exampleSettings : Settings
exampleSettings =
    Settings.default
        |> Settings.updateBoardConfigs (SafeZipper.fromList [ BoardConfig.DateBoardConfig BoardConfigHelpers.exampleDateBoardConfig ])
