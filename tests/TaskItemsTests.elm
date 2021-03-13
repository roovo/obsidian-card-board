module TaskItemsTests exposing (suite)

import Expect exposing (Expectation)
import Parser
import Parser.Advanced as Advanced
import TaskItem exposing (Completion(..), TaskItem)
import TaskItems
import Test exposing (..)


suite : Test
suite =
    describe "todo parsing"
        [ test "parses a single incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Advanced.run TaskItems.parser
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.title t)
                    |> Expect.equal [ "foo" ]
        , test "parses a contiguous block of TaskList items" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Advanced.run TaskItems.parser
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.title t)
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses non contiguous TaskList items" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Advanced.run TaskItems.parser
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
                    |> Advanced.run TaskItems.parser
                    |> Result.withDefault []
                    |> List.map (\t -> TaskItem.title t)
                    |> Expect.equal [ "foo", "bar", "baz" ]
        ]
