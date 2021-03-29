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
        ]


parsingToFix : Test
parsingToFix =
    describe "todo parsing it would be good to fix (save appending \n to end of string before parsing"
        [ test "FAILS to parse tasks when the last line is a non-task and has NO line ending" <|
            \() ->
                "- [ ] foo\na"
                    |> Parser.run (TaskItems.parser "" Nothing)
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
                    |> Parser.run (TaskItems.parser "" Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "parses a single incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItems.parser "" Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "parses a contiguous block of TaskList items" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser "" Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses non contiguous TaskList items" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Parser.run (TaskItems.parser "" Nothing)
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
                    |> Parser.run (TaskItems.parser "" Nothing)
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
                    |> Parser.run (TaskItems.parser "" Nothing)
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
                    |> Parser.run (TaskItems.parser "" Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks ignoring any that don't have a title" <|
            \() ->
                """
- [ ] foo
- [ ] 
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser "" Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the last line is a task and has NO line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar"
                    |> Parser.run (TaskItems.parser "" Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses tasks when the last line is a non-task and has a line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar\n\n## Log\n"
                    |> Parser.run (TaskItems.parser "" Nothing)
                    |> Result.withDefault []
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo", "bar" ]
        ]
