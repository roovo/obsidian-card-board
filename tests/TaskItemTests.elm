module TaskItemTests exposing (suite)

import Expect exposing (Expectation)
import Parser
import TaskItem exposing (Completion(..), TaskItem)
import Test exposing (..)


suite : Test
suite =
    describe "parsing"
        [ test "gets the title from an incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "gets the title from a complete TaskList item" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "gets the title from an upper case complete TaskList item" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "handles lots of whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "the task")
        , test "keeps trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "the task   ")
        , test "counts a task prefixed with '- [ ] ' as NOT completed" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok False)
        , test "counts a task prefixed with '- [x] ' as completed" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok True)
        , test "counts a task prefixed with '- [X] ' as completed" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok True)
        , test "only looks at the first line of a multline string" <|
            \() ->
                """- [X] foo
                - [ ] bar
                """
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "fails to parse a task which ends straight after the ']'" <|
            \() ->
                "- [X]"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with no title" <|
            \() ->
                "- [X] "
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with a '*' as the list marker" <|
            \() ->
                "* [X] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is no gap between the title and the ']'" <|
            \() ->
                "- [X]foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is whitespace before the list marker" <|
            \() ->
                " - [X] foo"
                    |> Parser.run (TaskItem.parser Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        ]
