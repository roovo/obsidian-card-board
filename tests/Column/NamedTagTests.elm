module Column.NamedTagTests exposing (suite)

import Column
import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
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
        , asColumn
        , init
        , isEnabled
        , name
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "Places an incomplete task item with a matching tag and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo #atag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with a matching tag (amongst others) and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "btag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and an incomplete sub-task with a matching tag" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an incomplete task item with no tags and an incomplete sub-task with a matching tag (amongst others)" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "btag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an incomplete task item with no tags and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with a non-matching tag and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo #xtag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with no tags and an incomplete sub-task with a non-matching tag" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #xtag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "CompletedInThisColumn a completed task item with a matching tag and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo #atag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "CompletedInThisColumn an incomplete task item with a matching tag (amongst others) and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "btag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "CompletedInThisColumn an incomplete task item with no tags and a completed sub-task with a matching tag" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "CompletedInThisColumn an incomplete task item with no tags and a completed sub-task with a matching tag (amongst others)" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "btag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with no tags and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong a completed task item with a non-matching tag and no sub-tasks" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo #xtag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong a completed task item with no tags and an incomplete sub-task with a non-matching tag" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar #xtag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with no tags and a completed sub-task with a non-matching tag" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #xtag")
                    |> Tuple.mapFirst NamedTagColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        ]


asColumn : Test
asColumn =
    describe "asColumn"
        [ test "sorts by due date then (case insensitive) title" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "atag" }
                    |> justAdd (taskItem "- [ ] f #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] d #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] E #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] c #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] a #atag @due(2022-01-03)")
                    |> justAdd (taskItem "- [ ] B #atag @due(2022-01-03)")
                    |> NamedTagColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


init : Test
init =
    describe "init"
        [ test "initializes with an empty TaskList" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "" }
                    |> NamedTagColumn.asColumn
                    |> Column.isEmpty
                    |> Expect.equal True
        ]


isEnabled : Test
isEnabled =
    describe "asColumn.isEnabled"
        [ test "is True" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "", tag = "" }
                    |> NamedTagColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal True
        ]


name : Test
name =
    describe "asColumn.name"
        [ test "is the displayTitle from the column config" <|
            \() ->
                NamedTagColumn.init defaultTagBoardConfig { displayTitle = "foo", tag = "" }
                    |> NamedTagColumn.asColumn
                    |> Column.name
                    |> Expect.equal "foo"
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultTagBoardConfig : TagBoard.Config
defaultTagBoardConfig =
    TagBoard.defaultConfig


justAdd : TaskItem -> NamedTagColumn -> NamedTagColumn
justAdd item column =
    column
        |> NamedTagColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
