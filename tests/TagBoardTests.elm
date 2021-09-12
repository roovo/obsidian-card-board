module TagBoardTests exposing (suite)

import Expect
import Parser
import TagBoard exposing (TagBoard)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Test exposing (..)


suite : Test
suite =
    concat
        [ columnsBasic
        , columnCompleted
        , columnOthers
        ]


columnsBasic : Test
columnsBasic =
    describe "columns - basic"
        [ test "returns no columns if none are defined in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar #bar
- [ ] baz #baz
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = []
                        }
                    |> TagBoard.columns
                    |> List.length
                    |> Expect.equal 0
        , test "returns a column for each one defined in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar #bar
- [ ] baz #baz
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns =
                                [ { tag = "foo", displayTitle = "" }
                                , { tag = "bar", displayTitle = "" }
                                , { tag = "baz", displayTitle = "" }
                                ]
                        }
                    |> TagBoard.columns
                    |> List.length
                    |> Expect.equal 3
        , test "ensures only uses columns with unique tags" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar #bar
- [ ] baz #baz
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns =
                                [ { tag = "foo", displayTitle = "foo" }
                                , { tag = "foo", displayTitle = "foo" }
                                , { tag = "bar", displayTitle = "bar" }
                                , { tag = "baz", displayTitle = "baz" }
                                , { tag = "bar", displayTitle = "bar" }
                                ]
                        }
                    |> TagBoard.columns
                    |> List.map Tuple.first
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "returns empty columns if there are no tasks with the given tags" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar #bar
- [ ] baz #baz
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "hello", displayTitle = "" } ]
                        }
                    |> TagBoard.columns
                    |> List.concatMap Tuple.second
                    |> List.length
                    |> Expect.equal 0
        , test "returns columns containing incomplete tasks with the given tags" <|
            \() ->
                """- [ ] foo #foo
- [x] bar1 #bar
- [ ] bar2 #bar
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "does not include tasks with tags that start with the given tag" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bart
- [ ] bar2 #bar
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "does not include tasks with tags with a trailing slash if not in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/
- [ ] bar2 #bar
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "includes tasks with tags with a trailing slash if in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/
- [ ] bar2 #bar
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar2" ]
        , test "includes tasks with a subtag if there is a trailing slash in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/one
- [ ] bar2 #bar
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar2" ]
        , test "prefixes the taskItem.inColumnId's with the title of the column" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/one
- [ ] bar2 #bar
"""
                    |> Parser.run (TaskList.parser "file_a" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "All Bar" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "All Bar"
                    |> List.map TaskItem.inColumnId
                    |> Expect.equal [ "bar/:file_a:2", "bar/:file_a:3" ]
        , test "sorts cards by title & due date" <|
            \() ->
                """- [ ] b #foo @due(2020-01-01)
- [ ] a #foo @due(2020-01-01)
- [ ] c #foo @due(2019-01-01)
"""
                    |> Parser.run (TaskList.parser "file_a" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | columns = [ { tag = "foo", displayTitle = "Foo tasks" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Foo tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        ]


columnCompleted : Test
columnCompleted =
    describe "columns - including completed tasks"
        [ test "adds a 'Done' column " <|
            \() ->
                ""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | includeCompleted = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> TagBoard.columns
                    |> List.map Tuple.first
                    |> Expect.equal [ "foo", "Done" ]
        , test "puts completed tasks with at least one of the tags in the 'Done' column " <|
            \() ->
                """- [ ] foo1 #foo
- [x] foo2 #foo/
- [x] foo3 #foo/one
- [x] foo4 #foo/one #foo
- [x] bar1 #bar
- [ ] bar2 #bar/
- [x] bar3 #bar/one
- [ ] baz1
- [x] baz2
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | includeCompleted = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Done"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar3", "foo4" ]
        , test "sorts cards by title & completion time" <|
            \() ->
                """- [x] c #foo @done(2019-01-01)
- [x] a #foo @done(2020-01-01)
- [x] b #foo @done(2020-01-01)
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | includeCompleted = True
                            , columns = [ { tag = "foo", displayTitle = "" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Done"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "b", "c" ]
        ]


columnOthers : Test
columnOthers =
    describe "columns - including other tasks"
        [ test "adds an 'Other' column " <|
            \() ->
                ""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> TagBoard.columns
                    |> List.map Tuple.first
                    |> Expect.equal [ "Others", "foo" ]
        , test "puts tasks with no tags or none of the current tags in the  'Other' column " <|
            \() ->
                """- [ ] foo1 #foo
- [ ] foo2 #foo/
- [ ] foo3 #foo/one
- [ ] bar1 #bar
- [ ] bar2 #bar/
- [ ] bar3 #bar/one
- [ ] baz1
- [x] baz2
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | includeOthers = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "baz1", "foo2", "foo3" ]
        , test "sorts cards by title & due date" <|
            \() ->
                """- [ ] b #foo @due(2020-01-01)
- [ ] a #foo @due(2020-01-01)
- [ ] c #foo @due(2019-01-01)
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "bar", displayTitle = "" } ]
                        }
                    |> TagBoard.columns
                    |> tasksInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        ]



-- HELPERS


defaultConfig : TagBoard.Config
defaultConfig =
    { columns = []
    , includeCompleted = False
    , includeOthers = False
    }


tasksInColumn : String -> List ( String, List TaskItem ) -> List TaskItem
tasksInColumn columnName tasksInColumns =
    tasksInColumns
        |> List.filter (\( c, ts ) -> c == columnName)
        |> List.concatMap Tuple.second
