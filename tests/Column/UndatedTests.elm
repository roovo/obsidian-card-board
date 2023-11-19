module Column.UndatedTests exposing (suite)

import Column
import Column.Undated as UndatedColumn exposing (UndatedColumn)
import ColumnNames exposing (ColumnNames)
import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import TaskItem exposing (TaskItem)
import Test exposing (..)


suite : Test
suite =
    concat
        [ addTaskItem
        , asColumn
        , init
        , isEnabled
        , name
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "Places an incomplete task item with no due date" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and incomplete sub-tasks with no due date" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and completed sub-tasks with no due date" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an incomplete task item with no due date and no sub-tasks if there is no undated column" <|
            \() ->
                UndatedColumn.init
                    { defaultDateBoardConfig | includeUndated = False }
                    defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with a due date" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo @due(2022-01-01)")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "Places an incomplete task item with no due date that has a sub-task with a due date" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar @due(2022-01-01)")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "CompletedInThisColumn a completed task item with no due date" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [x] foo")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with a a due date" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.addTaskItem (taskItem "- [x] foo @due(2022-01-01)")
                    |> Tuple.mapFirst UndatedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        ]


asColumn : Test
asColumn =
    describe "asColumn"
        [ test "sorts by (case insensitive) title" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> justAdd (taskItem "- [ ] f")
                    |> justAdd (taskItem "- [ ] d")
                    |> justAdd (taskItem "- [ ] E")
                    |> justAdd (taskItem "- [ ] c")
                    |> justAdd (taskItem "- [ ] a")
                    |> justAdd (taskItem "- [ ] B")
                    |> UndatedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B", "c", "d", "E", "f" ]
        ]


init : Test
init =
    describe "init"
        [ test "initializes with an empty TaskList" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.asColumn
                    |> Column.isEmpty
                    |> Expect.equal True
        ]


isEnabled : Test
isEnabled =
    describe "isEnabled"
        [ test "returns True if the config.includeUndated is True" <|
            \() ->
                UndatedColumn.init { defaultDateBoardConfig | includeUndated = True } defaultColumnNames
                    |> UndatedColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal True
        , test "returns False if the config.includeUndated is False" <|
            \() ->
                UndatedColumn.init { defaultDateBoardConfig | includeUndated = False } defaultColumnNames
                    |> UndatedColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal False
        ]


name : Test
name =
    describe "name"
        [ test "defaults to 'Undated'" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig defaultColumnNames
                    |> UndatedColumn.asColumn
                    |> Column.name
                    |> Expect.equal "Undated"
        , test "can be customized" <|
            \() ->
                UndatedColumn.init defaultDateBoardConfig { defaultColumnNames | undated = Just "Foo" }
                    |> UndatedColumn.asColumn
                    |> Column.name
                    |> Expect.equal "Foo"
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultDateBoardConfig : DateBoardConfig
defaultDateBoardConfig =
    let
        default : DateBoardConfig
        default =
            DateBoardConfig.default
    in
    { default | includeUndated = True }


justAdd : TaskItem -> UndatedColumn -> UndatedColumn
justAdd item column =
    column
        |> UndatedColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
