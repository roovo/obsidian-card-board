module TagBoardTests exposing (suite)

import Expect
import Parser
import TagBoard
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = []
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns =
                                [ { tag = "foo", displayTitle = "" }
                                , { tag = "bar", displayTitle = "" }
                                , { tag = "baz", displayTitle = "" }
                                ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns =
                                [ { tag = "foo", displayTitle = "foo" }
                                , { tag = "foo", displayTitle = "foo" }
                                , { tag = "bar", displayTitle = "bar" }
                                , { tag = "baz", displayTitle = "baz" }
                                , { tag = "bar", displayTitle = "bar" }
                                ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "hello", displayTitle = "" } ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar2" ]
        , test "sorts cards by title & due date" <|
            \() ->
                """- [ ] b #foo @due(2020-01-01)
- [ ] a #foo @due(2020-01-01)
- [ ] c #foo @due(2019-01-01)
"""
                    |> Parser.run (TaskList.parser "file_a" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "foo", displayTitle = "Foo tasks" } ]
                        }
                    |> tasksInColumn "Foo tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        ]


columnCompleted : Test
columnCompleted =
    describe "columns - including completed tasks"
        [ test "adds a 'Completed' column " <|
            \() ->
                ""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeCompleted = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Tuple.first
                    |> Expect.equal [ "foo", "Completed" ]
        , test "puts completed tasks with at least one of the tags in the 'Completed' column " <|
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
                    |> TagBoard.columns
                        { defaultConfig
                            | includeCompleted = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> tasksInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar3", "foo4" ]
        , test "sorts cards by title & completion time" <|
            \() ->
                """- [x] c #foo @completed(2019-01-01)
- [x] a #foo @completed(2020-01-01)
- [x] b #foo @completed(2020-01-01)
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeCompleted = True
                            , columns = [ { tag = "foo", displayTitle = "" } ]
                        }
                    |> tasksInColumn "Completed"
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
                    |> TagBoard.columns
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | includeOthers = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
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
                    |> TagBoard.columns
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "bar", displayTitle = "" } ]
                        }
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
    , title = "Tag Board Title"
    }


tasksInColumn : String -> List ( String, List TaskItem ) -> List TaskItem
tasksInColumn columnName tasksInColumns =
    tasksInColumns
        |> List.filter (\( c, ts ) -> c == columnName)
        |> List.concatMap Tuple.second
