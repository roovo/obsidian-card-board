module Column.CompletedTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import ColumnNames exposing (ColumnNames)
import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import TagBoardConfig exposing (TagBoardConfig)
import TagList
import TaskItem exposing (TaskItem)
import Test exposing (..)


suite : Test
suite =
    concat
        [ addTaskItem
        , asColumn
        , forTagBoard
        , isEnabled
        , name
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "does not add the task item if there are no PlacementResults" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        []
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "does not add the task item if all the PlacementResults are Placed" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.Placed, Column.Placed, Column.Placed ]
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "does not add the task item if all the PlacementResults are DoesNotBelong" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.DoesNotBelong, Column.DoesNotBelong, Column.DoesNotBelong ]
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "does not add the task item if there is one of each PlacementResult" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn, Column.DoesNotBelong, Column.Placed ]
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "adds the task item if all the PlacementResults are CompletedInThisColumn" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn, Column.CompletedInThisColumn, Column.CompletedInThisColumn ]
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add the task item if one of the PlacementResults is Placed" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn, Column.Placed, Column.CompletedInThisColumn ]
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "adds the task item if one of the PlacementResults is DoesNotBelong" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn, Column.DoesNotBelong, Column.CompletedInThisColumn ]
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        ]


asColumn : Test
asColumn =
    describe "asColumn"
        [ test "sorts by completion time then (case insensitive) title" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] f @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] d @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] E @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] c @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] B @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B", "c", "d", "E", "f" ]
        , test "limits the number retured as per the board config for a TagBoard" <|
            \() ->
                CompletedColumn.forTagBoard
                    { defaultTagBoardConfig | completedCount = 3 }
                    defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] f @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] d @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] E @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] c @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] B @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B", "c" ]
        , test "limits the number retured as per the board config for a DateBoard" <|
            \() ->
                CompletedColumn.forDateBoard
                    { defaultDateBoardConfig | completedCount = 3 }
                    defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] f @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] d @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] E @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] c @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] B @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B", "c" ]
        , test "removes top level tags from the Column TaskItems as defined in the config for all columns (if so configured)" <|
            \() ->
                CompletedColumn.forTagBoard
                    { defaultTagBoardConfig
                        | showColumnTags = False
                        , columns = [ { tag = "xtag", displayTitle = "" } ]
                    }
                    defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a #atag #xtag")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.topLevelTags
                    |> List.concatMap TagList.toList
                    |> Expect.equal [ "atag" ]
        , test "removes sub-task tags from the Column TaskItems as defined in the config for all columns (if so configured)" <|
            -- TODO: This is more observed than intended behaviour as I am not sure it this actually matters
            \() ->
                CompletedColumn.forTagBoard
                    { defaultTagBoardConfig
                        | showColumnTags = False
                        , columns = [ { tag = "xtag", displayTitle = "" } ]
                    }
                    defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a\n  - [ ] b #atag #xtag")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.tags
                    |> List.concatMap TagList.toList
                    |> Expect.equal [ "atag" ]
        , test "removes filter tags from the Column TaskItems (if so configured)" <|
            \() ->
                CompletedColumn.forTagBoard
                    { defaultTagBoardConfig
                        | showFilteredTags = False
                        , filters = [ FilterHelpers.tagFilter "xtag" ]
                    }
                    defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a #atag #xtag")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.topLevelTags
                    |> List.concatMap TagList.toList
                    |> Expect.equal [ "atag" ]
        , test "removes filter tags from the Column ub-askItems (if so configured)" <|
            -- TODO: This is more observed than intended behaviour as I am not sure it this actually matters
            \() ->
                CompletedColumn.forTagBoard
                    { defaultTagBoardConfig
                        | showFilteredTags = False
                        , filters = [ FilterHelpers.tagFilter "xtag" ]
                    }
                    defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a\n  - [ ] b #atag #xtag")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.tags
                    |> List.concatMap TagList.toList
                    |> Expect.equal [ "atag" ]
        , test "removes filter tags from the Column TaskItems (if so configured) for DateBoards" <|
            \() ->
                CompletedColumn.forDateBoard
                    { defaultDateBoardConfig
                        | showFilteredTags = False
                        , filters = [ FilterHelpers.tagFilter "xtag" ]
                    }
                    defaultColumnNames
                    |> CompletedColumn.addTaskItem
                        [ Column.CompletedInThisColumn ]
                        (taskItem "- [ ] a #atag #xtag")
                    |> CompletedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.topLevelTags
                    |> List.concatMap TagList.toList
                    |> Expect.equal [ "atag" ]
        ]


forTagBoard : Test
forTagBoard =
    describe "forTagBoard"
        [ test "initializes with an empty TaskList" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.asColumn
                    |> Column.isEmpty
                    |> Expect.equal True
        ]


isEnabled : Test
isEnabled =
    describe "isEnabled"
        [ test "returns True if the config.completedCount is 1" <|
            \() ->
                CompletedColumn.forTagBoard { defaultTagBoardConfig | completedCount = 1 } defaultColumnNames
                    |> CompletedColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal True
        , test "returns False if the config.completedCount is 0" <|
            \() ->
                CompletedColumn.forTagBoard { defaultTagBoardConfig | completedCount = 0 } defaultColumnNames
                    |> CompletedColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal False
        ]


name : Test
name =
    describe "name"
        [ test "defaults to 'Completed'" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig defaultColumnNames
                    |> CompletedColumn.asColumn
                    |> Column.name
                    |> Expect.equal "Completed"
        , test "can be customized" <|
            \() ->
                CompletedColumn.forTagBoard defaultTagBoardConfig { defaultColumnNames | completed = Just "Foo" }
                    |> CompletedColumn.asColumn
                    |> Column.name
                    |> Expect.equal "Foo"
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultDateBoardConfig : DateBoardConfig
defaultDateBoardConfig =
    DateBoardConfig.default


defaultTagBoardConfig : TagBoardConfig
defaultTagBoardConfig =
    TagBoardConfig.default


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
