module Column.UntaggedTests exposing (suite)

import Column
import Column.Untagged as UntaggedColumn
import ColumnNames exposing (ColumnNames)
import Expect
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ addTaskItem
        , init
        , isEnabled
        , name
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "Places an incomplete task item with no tags and no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and incomplete sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and completed sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an incomplete task item with a tag which has no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo #foo")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with no tags that has a tagged sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #bar")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "CompletedInThisColumn a completed task item with no tags and no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "CompletedInThisColumn a completed task with no tags and incomplete sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "CompletedInThisColumn a completed task item with no tags and completed sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [x] bar")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with a tag which has no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo #foo")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong a completed task item with no tags that has a tagged sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar #bar")
                    |> Tuple.mapFirst UntaggedColumn.taskList
                    |> Tuple.mapFirst TaskList.taskTitles
                    |> Expect.equal ( [], Column.DoesNotBelong )
        ]


init : Test
init =
    describe "init"
        [ test "initializes with an empty TaskList" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.taskList
                    |> Expect.equal TaskList.empty
        ]


isEnabled : Test
isEnabled =
    describe "isEnabled"
        [ test "returns True if the config.includeUntagged is True" <|
            \() ->
                UntaggedColumn.init
                    { defaultTagBoardConfig | includeUntagged = True }
                    defaultColumnNames
                    |> UntaggedColumn.isEnabled
                    |> Expect.equal True
        , test "returns False if the config.includeUntagged is False" <|
            \() ->
                UntaggedColumn.init
                    { defaultTagBoardConfig | includeUntagged = False }
                    defaultColumnNames
                    |> UntaggedColumn.isEnabled
                    |> Expect.equal False
        ]


name : Test
name =
    describe "name"
        [ test "defaults to 'Untagged'" <|
            \() ->
                UntaggedColumn.init
                    defaultTagBoardConfig
                    defaultColumnNames
                    |> UntaggedColumn.name
                    |> Expect.equal "Untagged"
        , test "can be customized" <|
            \() ->
                UntaggedColumn.init
                    defaultTagBoardConfig
                    { defaultColumnNames | untagged = Just "Foo" }
                    |> UntaggedColumn.name
                    |> Expect.equal "Foo"
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultTagBoardConfig : TagBoard.Config
defaultTagBoardConfig =
    TagBoard.defaultConfig


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
