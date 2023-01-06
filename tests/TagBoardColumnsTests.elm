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
import TagBoard
import TagBoardColumns exposing (TagBoardColumns)
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
                    }
                    |> TagBoardColumns.columns
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "Others", "name1", "name2" ]
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
                    }
                    |> TagBoardColumns.columns
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "name1", "name2" ]
        ]



-- HELPERS


defaultConfig : TagBoard.Config
defaultConfig =
    TagBoard.defaultConfig


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
