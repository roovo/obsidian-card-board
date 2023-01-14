module Column.DateTests exposing (suite)

import Column
import Column.Date as DateColumn exposing (DateColumn)
import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import TagList
import TaskItem exposing (TaskItem)
import Test exposing (..)
import TimeWithZone exposing (TimeWithZone)


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
        [ test "Places an incomplete task item with a matching due date" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString 0))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an incomplete task item with no due date" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem "- [ ] foo")
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item outside the top of the date range" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString 1))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item outside the bottom of the date range" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString -1))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "Places an incomplete task item at the top of the range when there is no range bottom" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Nothing, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString 0))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places an earlier incomplete task item when there is no range bottom" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Nothing, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString -22))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an incomplete task item beyond the end when there is no range bottom" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Nothing, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString 1))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "Places an incomplete task item at the bottom of the range when there is no range top" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Nothing }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString 0))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "Places a later incomplete task item when there is no range top" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Nothing }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString 15))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an incomplete task item beyond the bottom of the range when there is no range top" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Nothing }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString -1))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "Places a dated task item when there is no range bottom or top" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Nothing, to = Nothing }
                    |> DateColumn.addTaskItem now (taskItem ("- [ ] foo " ++ dueString 0))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], Column.Placed )
        , test "DoesNotBelong an un-dated task item when there is no range bottom or top" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Nothing, to = Nothing }
                    |> DateColumn.addTaskItem now (taskItem "- [ ] foo")
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "CompletedInThisColumn a completed task item with a matching due date" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [x] foo " ++ dueString 0))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with no due date" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem "- [x] foo")
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        , test "DoesNotBelong a completed task item outside the top of the date range" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 0 }
                    |> DateColumn.addTaskItem now (taskItem ("- [x] foo " ++ dueString 1))
                    |> Tuple.mapFirst DateColumn.asColumn
                    |> Tuple.mapFirst Column.items
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], Column.DoesNotBelong )
        ]


asColumn : Test
asColumn =
    describe "asColumn"
        [ test "sorts the Column TaskItems by due date then (case insensitive) title" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Nothing }
                    |> justAdd (taskItem ("- [ ] f " ++ dueString 0))
                    |> justAdd (taskItem ("- [ ] d " ++ dueString 1))
                    |> justAdd (taskItem ("- [ ] E " ++ dueString 0))
                    |> justAdd (taskItem ("- [ ] c " ++ dueString 1))
                    |> justAdd (taskItem ("- [ ] a " ++ dueString 2))
                    |> justAdd (taskItem ("- [ ] B " ++ dueString 2))
                    |> DateColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        , test "removes filter tags from the Column TaskItems (if so configured)" <|
            \() ->
                DateColumn.init
                    { defaultDateBoardConfig
                        | showFilteredTags = False
                        , filters = [ FilterHelpers.tagFilter "xtag" ]
                    }
                    { name = "foo", from = Just 0, to = Nothing }
                    |> justAdd (taskItem ("- [ ] a #atag #xtag " ++ dueString 0))
                    |> DateColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.topLevelTags
                    |> List.concatMap TagList.toList
                    |> Expect.equal [ "atag" ]
        , test "removes filter tags from the Column sub-askItems (if so configured)" <|
            -- TODO: This is more observed than intended behaviour as I am not sure it this actually matters
            \() ->
                DateColumn.init
                    { defaultDateBoardConfig
                        | showFilteredTags = False
                        , filters = [ FilterHelpers.tagFilter "xtag" ]
                    }
                    { name = "foo", from = Just 0, to = Nothing }
                    |> justAdd (taskItem ("- [ ] a " ++ dueString 0 ++ "\n  - [ ] b #atag #xtag"))
                    |> DateColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.tags
                    |> List.concatMap TagList.toList
                    |> Expect.equal [ "atag" ]
        ]


init : Test
init =
    describe "init"
        [ test "initializes with an empty TaskList" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 1 }
                    |> DateColumn.asColumn
                    |> Column.isEmpty
                    |> Expect.equal True
        ]


isEnabled : Test
isEnabled =
    describe "asColumn.isEnabled"
        [ test "is True" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 1 }
                    |> DateColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal True
        ]


name : Test
name =
    describe "asColumn.name"
        [ test "is the name from the config" <|
            \() ->
                DateColumn.init defaultDateBoardConfig { name = "foo", from = Just 0, to = Just 1 }
                    |> DateColumn.asColumn
                    |> Column.name
                    |> Expect.equal "foo"
        ]



-- HELPERS


dueString : Int -> String
dueString offset =
    "@due(" ++ DateTimeHelpers.offsetDateString offset ++ ")"


now : TimeWithZone
now =
    DateTimeHelpers.offsetNowWithZone 0


defaultDateBoardConfig : DateBoardConfig
defaultDateBoardConfig =
    DateBoardConfig.default


justAdd : TaskItem -> DateColumn -> DateColumn
justAdd item column =
    column
        |> DateColumn.addTaskItem now item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
