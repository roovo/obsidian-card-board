module TagBoardTests exposing (suite)

import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import Parser
import Set
import TagBoard
import TaskItem
import TaskList
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ columnsBasic
        , columnCompleted
        , columnOthers
        , columnUntagged
        , columnConfigsParserTest
        , encodeDecode
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
                    |> Parser.run TaskListHelpers.basicParser
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
                    |> Parser.run TaskListHelpers.basicParser
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
                    |> Parser.run TaskListHelpers.basicParser
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
                    |> Parser.run TaskListHelpers.basicParser
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
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "does not include tasks with tags that start with the given tag" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bart
- [ ] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "does not include tasks with tags with a trailing slash if not in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/
- [ ] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "includes tasks with tags with a trailing slash if in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/
- [ ] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar2" ]
        , test "includes tasks with a subtag if there is a trailing slash in the config" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/one
- [ ] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar2" ]
        , test "ensure only matches subtasks when using subtags" <|
            \() ->
                """- [ ] bar1 #at
- [ ] bar2 #at/foo
- [ ] bar3 #matt
- [ ] bar4 #atHome
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "at/", displayTitle = "At Tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "At Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar2" ]
        , test "does not remove tags that match the column's tag" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar/
  - [ ] bar1.1 #bar/baz
- [ ] bar2 #bar #baz
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Bar Tasks"
                    |> List.map TaskItem.tags
                    |> List.foldl Set.union Set.empty
                    |> Expect.equalSets (Set.fromList [ "bar/", "bar/baz", "bar", "baz" ])
        , test "sorts cards by title & due date" <|
            \() ->
                """- [ ] b #foo @due(2020-01-01)
- [ ] a #foo @due(2020-01-01)
- [ ] c #foo @due(2019-01-01)
"""
                    |> Parser.run (TaskList.parser "file_a" Nothing Set.empty 0)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | columns = [ { tag = "foo", displayTitle = "Foo tasks" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Foo tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        ]


columnCompleted : Test
columnCompleted =
    describe "columns - including completed tasks"
        [ test "adds a 'Completed' column " <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | completedCount = 1
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
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | completedCount = 99
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> BoardHelpers.tasksInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar3", "foo4" ]
        , test "sorts cards by title & completion time" <|
            \() ->
                """- [x] c #foo @completed(2019-01-01)
- [x] a #foo @completed(2020-01-01)
- [x] b #foo @completed(2020-01-01)
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | completedCount = 99
                            , columns = [ { tag = "foo", displayTitle = "" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "b", "c" ]
        , test "only shows the configured number of completed tasks" <|
            \() ->
                """- [x] complete1 #foo
- [x] complete2 #foo
- [x] complete3 #foo
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | completedCount = 2
                            , columns = [ { tag = "foo", displayTitle = "" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "complete1", "complete2" ]
        ]


columnOthers : Test
columnOthers =
    describe "'Other' column"
        [ test "adds an 'Other' column " <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Tuple.first
                    |> Expect.equal [ "Others", "foo" ]
        , test "puts tagged tasks with none of the current tags in the  'Other' column " <|
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
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeOthers = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> BoardHelpers.tasksInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo2", "foo3" ]
        , test "sorts cards by title & due date" <|
            \() ->
                """- [ ] b #foo @due(2020-01-01)
- [ ] a #foo @due(2020-01-01)
- [ ] c #foo @due(2019-01-01)
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "bar", displayTitle = "" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        ]


columnUntagged : Test
columnUntagged =
    describe "Untagged column"
        [ test "adds an 'Untagged' column " <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeUntagged = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Tuple.first
                    |> Expect.equal [ "Untagged", "foo" ]
        , test "puts tasks with NO tags in the  'Untagged' column " <|
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
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeUntagged = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> BoardHelpers.tasksInColumn "Untagged"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "baz1" ]
        , test "sorts cards by title & due date" <|
            \() ->
                """- [ ] b @due(2020-01-01)
- [ ] a @due(2020-01-01)
- [ ] c @due(2019-01-01)
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.columns
                        { defaultConfig
                            | includeUntagged = True
                            , columns = [ { tag = "bar", displayTitle = "" } ]
                        }
                    |> BoardHelpers.tasksInColumn "Untagged"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        ]


columnConfigsParserTest : Test
columnConfigsParserTest =
    describe "parsing tags with headers"
        [ test "parses an empty string to an empty config" <|
            \() ->
                ""
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal (Ok [])
        , test "parses an string containing a single word" <|
            \() ->
                "foo"
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoard.ColumnConfig "foo" "Foo" ])
        , test "parses '#foo'" <|
            \() ->
                "#foo"
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoard.ColumnConfig "foo" "Foo" ])
        , test "parses '#foo/bar'" <|
            \() ->
                "#foo/bar"
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoard.ColumnConfig "foo/bar" "Foo bar" ])
        , test "parses '#foo/bar baz'" <|
            \() ->
                "#foo/bar baz"
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoard.ColumnConfig "foo/bar" "baz" ])
        , test "parses '#foo/bar baz bax'" <|
            \() ->
                "#foo/bar baz bax"
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoard.ColumnConfig "foo/bar" "baz bax" ])
        , test "parses '   #foo/bar     baz     bax    '" <|
            \() ->
                "   #foo/bar     baz     bax    "
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoard.ColumnConfig "foo/bar" "baz bax" ])
        , test "parses multilines" <|
            \() ->
                """#foo     bar     baz
#aa"""
                    |> Parser.run TagBoard.columnConfigsParser
                    |> Expect.equal
                        (Ok
                            [ TagBoard.ColumnConfig "foo" "bar baz"
                            , TagBoard.ColumnConfig "aa" "Aa"
                            ]
                        )
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleConfig
                    |> TsEncode.runExample TagBoard.configEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder TagBoard.configDecoder
                    |> .decoded
                    |> Expect.equal (Ok exampleConfig)

        -- , test "builds the correct tsType" <|
        --     \() ->
        --         ""
        --             |> DecodeHelpers.runDecoder TagBoard.configDecoder
        --             |> .tsType
        --             |> Expect.equal ""
        ]



-- HELPERS


defaultConfig : TagBoard.Config
defaultConfig =
    BoardConfigHelpers.defaultTagBoardConfig


exampleConfig : TagBoard.Config
exampleConfig =
    BoardConfigHelpers.exampleTagBoardConfig
