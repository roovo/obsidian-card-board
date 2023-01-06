module TagBoardConfigTests exposing (suite)

import Column
import ColumnNames exposing (ColumnNames)
import DataviewTaskCompletion
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import Parser
import TagBoardConfig exposing (TagBoardConfig)
import TagList
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns =
                                [ { tag = "foo", displayTitle = "foo" }
                                , { tag = "foo", displayTitle = "foo" }
                                , { tag = "bar", displayTitle = "bar" }
                                , { tag = "baz", displayTitle = "baz" }
                                , { tag = "bar", displayTitle = "bar" }
                                ]
                        }
                    |> List.map Column.name
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "returns empty columns if there are no tasks with the given tags" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar #bar
- [ ] baz #baz
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "hello", displayTitle = "" } ]
                        }
                    |> List.concatMap Column.items
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "returns columns containing incomplete sub tasks with the given tag if the top level task is incomplete" <|
            \() ->
                """- [ ] foo #foo
  - [ ] bar1 #bar
- [x] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not return column containing complete sub-task with the given tag if the top level task is incomplete" <|
            \() ->
                """- [ ] foo #foo
  - [x] bar1 #bar
- [x] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "does not return column containing incomplete sub tasks with the given tag if the top level task is complete" <|
            \() ->
                """- [x] foo #foo
  - [ ] bar1 #bar
- [x] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "does not include tasks with tags that start with the given tag" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bart
- [ ] bar2 #bar
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "bar/", displayTitle = "Bar Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Bar Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar1", "bar2" ]
        , test "ensure only matches subtags when using subtags" <|
            \() ->
                """- [ ] bar1 #at
- [ ] bar2 #at/foo
- [ ] bar3 #matt
- [ ] bar4 #atHome
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "at/foo", displayTitle = "At Tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "At Tasks"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "bar2" ]
        , test "removes exact matches of tags defined in config.filters from all task items if the config says not to show them" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar #col1
  - [ ] bar1.1 #bar #bar/baz #subtask
- [ ] bar2 #bar #bar/ #col2
- [ ] foo #foo #check
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | filters = [ FilterHelpers.tagFilter "bar" ]
                            , filterPolarity = Filter.Allow
                            , showFilteredTags = False
                            , columns =
                                [ { tag = "col1", displayTitle = "Col 1" }
                                , { tag = "col2", displayTitle = "Col 2" }
                                ]
                        }
                    |> BoardHelpers.thingsInColumns [ "Col 1", "Col 2" ]
                    |> List.map TaskItem.tags
                    |> List.foldl TagList.append TagList.empty
                    |> TagList.toList
                    |> List.sort
                    |> Expect.equal [ "bar/", "bar/baz", "col1", "col2", "subtask" ]
        , test "removes exact matches of tags defined in config.columns from all task items if the config says not to show them" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar1 #bar #col1
  - [ ] bar1.1 #bar #bar/baz #subtask
- [ ] bar2 #bar #bar/ #col2
- [ ] foo #foo #check
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns =
                                [ { tag = "col1", displayTitle = "Col 1" }
                                , { tag = "col2", displayTitle = "Col 2" }
                                ]
                            , showColumnTags = False
                        }
                    |> BoardHelpers.thingsInColumns [ "Col 1", "Col 2" ]
                    |> List.map TaskItem.tags
                    |> List.foldl TagList.append TagList.empty
                    |> TagList.toList
                    |> List.sort
                    |> Expect.equal [ "bar", "bar", "bar/", "bar/baz", "subtask" ]
        , test "sorts cards by title & due date" <|
            \() ->
                """- [ ] b #foo @due(2020-01-01)
- [ ] a #foo @due(2020-01-01)
- [ ] c #foo @due(2019-01-01)
"""
                    |> Parser.run (TaskList.parser DataviewTaskCompletion.NoCompletion "file_a" Nothing TagList.empty 0)
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns = [ { tag = "foo", displayTitle = "Foo tasks" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Foo tasks"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | completedCount = 1
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Column.name
                    |> Expect.equal [ "foo", "Completed" ]
        , test "can customize the 'Completed' column name" <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        { defaultColumnNames | completed = Just "xxx" }
                        { defaultConfig
                            | completedCount = 1
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Column.name
                    |> Expect.equal [ "foo", "xxx" ]
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | completedCount = 99
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> BoardHelpers.thingsInColumn "Completed"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | completedCount = 99
                            , columns = [ { tag = "foo", displayTitle = "" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Completed"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | completedCount = 2
                            , columns = [ { tag = "foo", displayTitle = "" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "complete1", "complete2" ]
        , test "removes exact matches of tags defined in config.filters if the config says not to show them" <|
            \() ->
                """- [x] complete1 #foo #bar
  - [x] complete2 #foo #bar2
- [x] complete3 #foo
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | filters = [ FilterHelpers.tagFilter "foo" ]
                            , filterPolarity = Filter.Allow
                            , showFilteredTags = False
                            , columns = [ { tag = "foo", displayTitle = "" } ]
                            , completedCount = 10
                        }
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.tags
                    |> List.foldl TagList.append TagList.empty
                    |> TagList.toList
                    |> List.sort
                    |> Expect.equal [ "bar", "bar2" ]
        , test "removes exact matches of tags defined in config.columns from all task items if the config says not to show them" <|
            \() ->
                """- [x] complete1 #foo #bar #col1
  - [x] complete2 #foo #bar2
- [x] complete3 #foo #col2
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | columns =
                                [ { tag = "col1", displayTitle = "Col 1" }
                                , { tag = "col2", displayTitle = "Col 2" }
                                ]
                            , showColumnTags = False
                            , completedCount = 10
                        }
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.tags
                    |> List.foldl TagList.append TagList.empty
                    |> TagList.toList
                    |> List.sort
                    |> Expect.equal [ "bar", "bar2", "foo", "foo" ]
        ]


columnOthers : Test
columnOthers =
    describe "'Other' column"
        [ test "adds an 'Other' column " <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Column.name
                    |> Expect.equal [ "Others", "foo" ]
        , test "allows the 'Other' column name to be customized" <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        { defaultColumnNames | others = Just "xxx" }
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Column.name
                    |> Expect.equal [ "xxx", "foo" ]
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | includeOthers = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> BoardHelpers.thingsInColumn "Others"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | includeOthers = True
                            , columns = [ { tag = "bar", displayTitle = "" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        , test "removes exact matches of tags defined in config.filters if the config says not to show them" <|
            \() ->
                """- [ ] incomplete1 #foo
  - [ ] incomplete2 #foo #bar
- [ ] incomplete3 #foo #baz
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | filters = [ FilterHelpers.tagFilter "foo" ]
                            , filterPolarity = Filter.Allow
                            , showFilteredTags = False
                            , columns = [ { tag = "bar", displayTitle = "" } ]
                            , includeOthers = True
                        }
                    |> BoardHelpers.thingsInColumn "Others"
                    |> List.map TaskItem.tags
                    |> List.foldl TagList.append TagList.empty
                    |> TagList.toList
                    |> List.sort
                    |> Expect.equal [ "baz" ]
        ]


columnUntagged : Test
columnUntagged =
    describe "Untagged column"
        [ test "adds an 'Untagged' column " <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | includeUntagged = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "foo" ]
        , test "allow the 'Untagged' column name to be customized" <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TagBoardConfig.columns
                        { defaultColumnNames | untagged = Just "xxx" }
                        { defaultConfig
                            | includeUntagged = True
                            , columns = [ { tag = "foo", displayTitle = "foo" } ]
                        }
                    |> List.map Column.name
                    |> Expect.equal [ "xxx", "foo" ]
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | includeUntagged = True
                            , columns =
                                [ { tag = "bar/", displayTitle = "" }
                                , { tag = "foo", displayTitle = "" }
                                ]
                        }
                    |> BoardHelpers.thingsInColumn "Untagged"
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
                    |> TagBoardConfig.columns
                        ColumnNames.default
                        { defaultConfig
                            | includeUntagged = True
                            , columns = [ { tag = "bar", displayTitle = "" } ]
                        }
                    |> BoardHelpers.thingsInColumn "Untagged"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c", "a", "b" ]
        ]


columnConfigsParserTest : Test
columnConfigsParserTest =
    describe "parsing tags with headers"
        [ test "parses an empty string to an empty config" <|
            \() ->
                ""
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [])
        , test "parses an string containing a single word" <|
            \() ->
                "foo"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.ColumnConfig "foo" "Foo" ])
        , test "parses '#foo'" <|
            \() ->
                "#foo"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.ColumnConfig "foo" "Foo" ])
        , test "parses '#foo/bar'" <|
            \() ->
                "#foo/bar"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.ColumnConfig "foo/bar" "Foo bar" ])
        , test "parses '#foo/bar baz'" <|
            \() ->
                "#foo/bar baz"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.ColumnConfig "foo/bar" "baz" ])
        , test "parses '#foo/bar baz bax'" <|
            \() ->
                "#foo/bar baz bax"
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.ColumnConfig "foo/bar" "baz bax" ])
        , test "parses '   #foo/bar     baz     bax    '" <|
            \() ->
                "   #foo/bar     baz     bax    "
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal (Ok [ TagBoardConfig.ColumnConfig "foo/bar" "baz bax" ])
        , test "parses multilines" <|
            \() ->
                """#foo     bar     baz
#aa"""
                    |> Parser.run TagBoardConfig.columnConfigsParser
                    |> Expect.equal
                        (Ok
                            [ TagBoardConfig.ColumnConfig "foo" "bar baz"
                            , TagBoardConfig.ColumnConfig "aa" "Aa"
                            ]
                        )
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleConfig
                    |> TsEncode.runExample TagBoardConfig.configEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder TagBoardConfig.configDecoder_v_0_4_0
                    |> .decoded
                    |> Expect.equal (Ok exampleConfig)
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultConfig : TagBoardConfig
defaultConfig =
    BoardConfigHelpers.defaultTagBoardConfig


exampleConfig : TagBoardConfig
exampleConfig =
    BoardConfigHelpers.exampleTagBoardConfig
