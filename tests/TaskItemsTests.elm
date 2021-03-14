module TaskItemsTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Parser
import TaskItem exposing (Completion(..), TaskItem)
import TaskItems
import Test exposing (..)


suite : Test
suite =
    describe "todo parsing"
        [ test "parses an empty file" <|
            \() ->
                ""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.title t)
                    |> Expect.equal []
        , test "parses a single incomplete TaskList item" <|
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
        , test "pareses completion" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal [ False, True, True ]
        , test "pareses undated tasks" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskItems.parser Nothing)
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.due t)
                    |> Expect.equal [ Nothing, Nothing, Nothing ]
        , test "pareses dated tasks" <|
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
