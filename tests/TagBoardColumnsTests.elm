module TagBoardColumnsTests exposing (suite)

import Column
import ColumnNames exposing (ColumnNames)
import DataviewTaskCompletion
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import Parser
import TagBoardColumns exposing (TagBoardColumns)
import TagBoardConfig exposing (TagBoardConfig)
import TagList
import TaskItem exposing (TaskItem)
import TaskList
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ columns
        , addTaskItem
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "adds a task item with no tags to the untagged column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [ ] foo")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Untagged"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add a completed task item with no tags to the untagged column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [x] foo")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Untagged"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "adds a task item with a different tag to the others column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [ ] foo #tagA")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add a completedtask item with a different tag to the others column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [x] foo #tagA")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "adds a task item with a named tag to the specified column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [ ] foo #tag1")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "name1"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add a completed task item with a named tag to the specified column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [x] foo #tag1")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "name1"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "adds a completed task item with no tags to the completed column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [x] foo")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds a completedtask item with a different tag to the completed column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [x] foo #tagA")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds a completed task item with a named tag to the completed column" <|
            \() ->
                emptyTagBoardColumns
                    |> TagBoardColumns.addTaskItem (taskItem "- [x] foo #tag1")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add a completed task item with no tag to the completed column" <|
            \() ->
                columnsWithNoUntagged
                    |> TagBoardColumns.addTaskItem (taskItem "- [x] foo")
                    |> TagBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        ]


columns : Test
columns =
    describe "columns"
        [ test "returns no columns if none are defined in the config" <|
            \() ->
                TagBoardColumns.init
                    ColumnNames.default
                    { defaultConfig
                        | columns = []
                        , completedCount = 0
                    }
                    |> TagBoardColumns.columns
                    |> List.length
                    |> Expect.equal 0
        , test "returns columns defined in the config" <|
            \() ->
                TagBoardColumns.init
                    ColumnNames.default
                    { defaultConfig
                        | columns =
                            [ { displayTitle = "name1", tag = "tag1" }
                            , { displayTitle = "name2", tag = "tag2" }
                            ]
                        , includeOthers = True
                        , includeUntagged = True
                        , completedCount = 10
                    }
                    |> TagBoardColumns.columns
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "Others", "name1", "name2", "Completed" ]
        , test "ensures there is only one column per tag" <|
            \() ->
                TagBoardColumns.init
                    ColumnNames.default
                    { defaultConfig
                        | columns =
                            [ { displayTitle = "name1", tag = "tag1" }
                            , { displayTitle = "name2", tag = "tag2" }
                            , { displayTitle = "name3", tag = "tag1" }
                            , { displayTitle = "name4", tag = "tag2" }
                            ]
                        , includeOthers = False
                        , includeUntagged = True
                        , completedCount = 0
                    }
                    |> TagBoardColumns.columns
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "name1", "name2" ]
        ]



-- HELPERS


columnsWithNoUntagged : TagBoardColumns
columnsWithNoUntagged =
    TagBoardColumns.init
        ColumnNames.default
        { defaultConfig
            | columns =
                [ { displayTitle = "name1", tag = "tag1" }
                , { displayTitle = "name2", tag = "tag2" }
                ]
            , includeOthers = True
            , includeUntagged = False
        }


defaultConfig : TagBoardConfig
defaultConfig =
    TagBoardConfig.default


emptyTagBoardColumns : TagBoardColumns
emptyTagBoardColumns =
    TagBoardColumns.init
        ColumnNames.default
        { defaultConfig
            | columns =
                [ { displayTitle = "name1", tag = "tag1" }
                , { displayTitle = "name2", tag = "tag2" }
                ]
            , includeOthers = True
            , includeUntagged = True
        }


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
