module Column.UntaggedTests exposing (suite)

import Column
import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import ColumnNames exposing (ColumnNames)
import Expect
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import TagBoardConfig exposing (TagBoardConfig)
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
        [ test "Places an incomplete task item with no tags and no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and incomplete sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and completed sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an incomplete task item with no tags and no sub-tasks if there is no untagged column" <|
            \() ->
                UntaggedColumn.init
                    { defaultTagBoardConfig | includeUntagged = False }
                    defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with a tag which has no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo #foo")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with no tags that has a tagged sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #bar")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "CompletedInThisColumn a completed task item with no tags and no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "CompletedInThisColumn a completed task with no tags and incomplete sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "CompletedInThisColumn a completed task item with no tags and completed sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [x] bar")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with a tag which has no sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo #foo")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong a completed task item with no tags that has a tagged sub-tasks" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar #bar")
                    |> Tuple.mapFirst UntaggedColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        ]


asColumn : Test
asColumn =
    describe "asColumn"
        [ test "sorts by due date then (case insensitive) title" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> justAdd (taskItem "- [ ] f @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] d @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] E @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] c @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] a @due(2022-01-03)")
                    |> justAdd (taskItem "- [ ] B @due(2022-01-03)")
                    |> UntaggedColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


init : Test
init =
    describe "init"
        [ test "initializes with an empty TaskList" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.asColumn
                    |> Column.isEmpty
                    |> Expect.equal True
        ]


isEnabled : Test
isEnabled =
    describe "isEnabled"
        [ test "returns True if the config.includeUntagged is True" <|
            \() ->
                UntaggedColumn.init { defaultTagBoardConfig | includeUntagged = True } defaultColumnNames
                    |> UntaggedColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal True
        , test "returns False if the config.includeUntagged is False" <|
            \() ->
                UntaggedColumn.init { defaultTagBoardConfig | includeUntagged = False } defaultColumnNames
                    |> UntaggedColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal False
        ]


name : Test
name =
    describe "name"
        [ test "defaults to 'Untagged'" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.asColumn
                    |> Column.name
                    |> Expect.equal "Untagged"
        , test "can be customized" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig { defaultColumnNames | untagged = Just "Foo" }
                    |> UntaggedColumn.asColumn
                    |> Column.name
                    |> Expect.equal "Foo"
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultTagBoardConfig : TagBoardConfig
defaultTagBoardConfig =
    let
        default : TagBoardConfig
        default =
            TagBoardConfig.default
    in
    { default | includeUntagged = True }


justAdd : TaskItem -> UntaggedColumn -> UntaggedColumn
justAdd item column =
    column
        |> UntaggedColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
