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
        , columnsOthers
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
                    |> TagBoard.fill { defaultConfig | columns = [] }
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
                    |> TagBoard.fill { defaultConfig | columns = [ "foo", "bar", "baz" ] }
                    |> TagBoard.columns
                    |> List.length
                    |> Expect.equal 3
        , test "ensures only uses columns with unique names" <|
            \() ->
                """- [ ] foo #foo
- [ ] bar #bar
- [ ] baz #baz
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill { defaultConfig | columns = [ "foo", "foo", "bar", "baz", "bar" ] }
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
                    |> TagBoard.fill { defaultConfig | columns = [ "hello" ] }
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
                    |> TagBoard.fill { defaultConfig | columns = [ "bar" ] }
                    |> TagBoard.columns
                    |> tasksInColumn "bar"
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
                    |> TagBoard.fill { defaultConfig | columns = [ "bar" ] }
                    |> TagBoard.columns
                    |> tasksInColumn "bar"
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
                    |> TagBoard.fill { defaultConfig | columns = [ "bar" ] }
                    |> TagBoard.columns
                    |> tasksInColumn "bar"
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
                    |> TagBoard.fill { defaultConfig | columns = [ "bar/" ] }
                    |> TagBoard.columns
                    |> tasksInColumn "bar/"
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
                    |> TagBoard.fill { defaultConfig | columns = [ "bar/" ] }
                    |> TagBoard.columns
                    |> tasksInColumn "bar/"
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
                    |> TagBoard.fill { defaultConfig | columns = [ "bar/" ] }
                    |> TagBoard.columns
                    |> tasksInColumn "bar/"
                    |> List.map TaskItem.inColumnId
                    |> Expect.equal [ "bar/:file_a:2", "bar/:file_a:3" ]
        ]


columnsOthers : Test
columnsOthers =
    describe "columns - including other tasks"
        [ test "adds an 'Other' column " <|
            \() ->
                ""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TagBoard.fill { defaultConfig | includeOthers = True, columns = [ "foo" ] }
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
                    |> TagBoard.fill { defaultConfig | includeOthers = True, columns = [ "bar/", "foo" ] }
                    |> TagBoard.columns
                    |> tasksInColumn "Others"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo2", "foo3", "baz1" ]
        ]



-- HELPERS


defaultConfig : TagBoard.Config
defaultConfig =
    { columns = []
    , includeOthers = False
    }


tasksInColumn : String -> List ( String, List TaskItem ) -> List TaskItem
tasksInColumn columnName tasksInColumns =
    tasksInColumns
        |> List.filter (\( c, ts ) -> c == columnName)
        |> List.concatMap Tuple.second
