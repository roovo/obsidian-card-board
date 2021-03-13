module TaskItemTests exposing (suite)

import Expect exposing (Expectation)
import Parser
import Parser.Advanced as Advanced
import TaskItem exposing (Completion(..), TaskItem)
import Test exposing (..)


suite : Test
suite =
    describe "todo parsing"
        [ test "gets the title from an incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "gets the title from a complete TaskList item" <|
            \() ->
                "- [x] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "gets the title from an upper case complete TaskList item" <|
            \() ->
                "- [X] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "handles lots of whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "the task")
        , test "keeps trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "the task   ")
        , test "counts a task prefixed with '- [ ] ' as NOT completed" <|
            \() ->
                "- [ ] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok False)
        , test "counts a task prefixed with '- [x] ' as completed" <|
            \() ->
                "- [x] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok True)
        , test "counts a task prefixed with '- [X] ' as completed" <|
            \() ->
                "- [X] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok True)
        , test "only looks at the first line of a multline string" <|
            \() ->
                """- [X] foo
                - [ ] bar
                """
                    |> Advanced.run TaskItem.parser
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "fails to parse a task which ends straight after the ']'" <|
            \() ->
                "- [X]"
                    |> Advanced.run TaskItem.parser
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with no title" <|
            \() ->
                "- [X] "
                    |> Advanced.run TaskItem.parser
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with a '*' as the list marker" <|
            \() ->
                "* [X] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is no gap between the title and the ']'" <|
            \() ->
                "- [X]foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is whitespace before the list marker" <|
            \() ->
                " - [X] foo"
                    |> Advanced.run TaskItem.parser
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        ]
