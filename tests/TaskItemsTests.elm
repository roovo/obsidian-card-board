module TaskItemsTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Parser
import TaskItem exposing (Completion(..), TaskItem)
import TaskItems
import Test exposing (..)


suite : Test
suite =
    concat
        [ parsing
        , parsingToFix
        , filtering
        ]


parsingToFix : Test
parsingToFix =
    describe "todo parsing it would be good to fix (save appending \n to end of string before parsing"
        [ test "FAILS to parse tasks when the last line is a non-task and has NO line ending" <|
            \() ->
                "- [ ] foo\na"
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal []
        ]


parsing : Test
parsing =
    describe "todo parsing"
        [ test "parses an empty file" <|
            \() ->
                ""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "parses a single incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "parses a contiguous block of TaskList items" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses non contiguous TaskList items" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses TaskList items with non-tasks interspersed" <|
            \() ->
                """- [ ] foo

not a task

- [x] bar

- [X] baz

"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "ignores indented tasks" <|
            \() ->
                """- [ ] foo

not a task

- [x] bar

- [X] baz
  - [ ] a subtask

"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the first line of the file is blank" <|
            \() ->
                """
- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the last line is a task and has NO line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar"
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses tasks when the last line is a non-task and has a line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar\n\n## Log\n"
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses completion" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.isCompleted
                    |> Expect.equal [ False, True, True ]
        , test "parses undated tasks" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.due
                    |> Expect.equal [ Nothing, Nothing, Nothing ]
        , test "parses dated tasks" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser <| Just "2020-02-22")
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.due t |> Maybe.map Date.toIsoString)
                    |> Expect.equal [ Just "2020-02-22", Just "2020-02-22", Just "2020-02-22" ]
        ]


filtering : Test
filtering =
    describe "filtering"
        [ test "undated" <|
            \() ->
                parsedFiles
                    |> TaskItems.undated
                    |> List.map TaskItem.title
                    |> Expect.equal [ "undated incomplete" ]
        , test "forToday" <|
            \() ->
                parsedFiles
                    |> TaskItems.forToday todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "yesterday incomplete", "today incomplete" ]
        , test "forTommorrow" <|
            \() ->
                parsedFiles
                    |> TaskItems.forTomorrow todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "tomorrow incomplete" ]
        , test "forFuture" <|
            \() ->
                parsedFiles
                    |> TaskItems.forFuture todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "future incomplete" ]
        , test "completed" <|
            \() ->
                parsedFiles
                    |> TaskItems.completed
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "undated complete"
                        , "yesterday complete"
                        , "today complete"
                        , "tomorrow complete"
                        , "future complete"
                        ]
        ]


parsedFiles : List TaskItem
parsedFiles =
    taskFiles
        |> List.map (\( d, ts ) -> Parser.run (TaskItems.parser d) ts)
        |> List.concatMap (Result.withDefault [])


todayAsDate : Date.Date
todayAsDate =
    today
        |> Date.fromIsoString
        |> Result.map Date.toRataDie
        |> Result.withDefault 0
        |> Date.fromRataDie


taskFiles : List ( Maybe String, String )
taskFiles =
    [ ( Nothing, """
- [ ] undated incomplete
- [x] undated complete
""" )
    , ( Just yesterday, """
- [ ] yesterday incomplete
- [x] yesterday complete
""" )
    , ( Just today, """
- [ ] today incomplete
- [x] today complete
""" )
    , ( Just tomorrow, """
- [ ] tomorrow incomplete
- [x] tomorrow complete
""" )
    , ( Just future, """
- [ ] future incomplete
- [x] future complete
""" )
    ]


yesterday : String
yesterday =
    "2020-06-19"


today : String
today =
    "2020-06-20"


tomorrow : String
tomorrow =
    "2020-06-21"


future : String
future =
    "2020-06-22"
