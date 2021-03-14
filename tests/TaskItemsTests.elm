module TaskItemsTests exposing (suite)

import Expect exposing (Expectation)
import Parser
import TaskItem exposing (Completion(..), TaskItem)
import TaskItems
import Test exposing (..)


suite : Test
suite =
    describe "todo parsing"
        [ test "parses a single incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.title t)
                    |> Expect.equal [ "foo" ]
        , test "parses a contiguous block of TaskList items" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.title t)
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses non contiguous TaskList items" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.title t)
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
                    |> List.map (\t -> TaskItem.title t)
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
                    |> List.map (\t -> TaskItem.title t)
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
                    |> List.map (\t -> TaskItem.title t)
                    |> Expect.equal [ "foo", "bar", "baz" ]
        ]
