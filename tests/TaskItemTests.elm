module TaskItemTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Parser exposing ((|=))
import TaskItem exposing (Completion(..), TaskItem)
import TaskList exposing (TaskList)
import Test exposing (..)


suite : Test
suite =
    concat
        [ info
        , parsing
        , toString
        , transformation
        , done
        ]


done : Test
done =
    describe "completion"
        [ test "returns Incomplete for an incomplete task" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns Complete for a completed task with no @done() date" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        ]


info : Test
info =
    describe "info"
        [ test "due returns Nothing if there was no date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.due t)
                    |> Expect.equal (Ok Nothing)
        , test "due returns Nothing if the date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "not a date")
                    |> Result.map (\t -> TaskItem.due t)
                    |> Expect.equal (Ok Nothing)
        , test "due returns Just the date if the date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "2020-01-07")
                    |> Result.map (\t -> TaskItem.due t)
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737431)
        , test "isCompleted returns False if the checkbox is NOT checked" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok False)
        , test "isCompleted returns True if the checkbox is checked with an x" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok True)
        , test "isCompleted returns True if the checkbox is checked with an X" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok True)
        , test "isDated returns False if there was no date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.isDated t)
                    |> Expect.equal (Ok False)
        , test "isDated returns False if the date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "not a date")
                    |> Result.map (\t -> TaskItem.isDated t)
                    |> Expect.equal (Ok False)
        , test "isDated returns True if the date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "2020-01-07")
                    |> Result.map (\t -> TaskItem.isDated t)
                    |> Expect.equal (Ok True)
        , test "filePath returns filePath" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map TaskItem.filePath
                    |> Expect.equal (Ok "File A")
        , test "id returns filePath:lineNumber" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map TaskItem.id
                    |> Expect.equal (Ok "File A:1")
        , test "isFromFile returns False if the filenames do NOT match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (\t -> TaskItem.isFromFile "File B" t)
                    |> Expect.equal (Ok False)
        , test "isFromFile returns True if the filenames do match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (\t -> TaskItem.isFromFile "File A" t)
                    |> Expect.equal (Ok True)
        , test "isFromFile returns True if the filenames are both blank" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.isFromFile "" t)
                    |> Expect.equal (Ok True)
        , test "isFromFile returns False if the filenames do NOT match case" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (\t -> TaskItem.isFromFile "File a" t)
                    |> Expect.equal (Ok False)
        , test "isFromFile returns False if the filenames are a partial match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (\t -> TaskItem.isFromFile "File" t)
                    |> Expect.equal (Ok False)
        ]


transformation : Test
transformation =
    describe "transformation"
        [ test "toggling a complete task produces one marked as complete" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toggleCompletion t)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok False)
        , test "toggling an incomplete task produces one marked as complete" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toggleCompletion t)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok True)
        ]


parsing : Test
parsing =
    describe "parsing"
        [ test "gets the title from an incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "gets the title from a complete TaskList item" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "gets the title from an upper case complete TaskList item" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "handles lots of whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "the task")
        , test "keeps trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "the task   ")
        , test "only looks at the first line of a multline string" <|
            \() ->
                "- [X] foo\n- [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
        , test "consumes <eol> character" <|
            \() ->
                "- [X] foo\n- [ ] bar"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItem.parser "" Nothing
                            |= TaskItem.parser "" Nothing
                        )
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "fails to parse a task which ends straight after the ']'" <|
            \() ->
                "- [X]"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with no title" <|
            \() ->
                "- [X] "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with a '*' as the list marker" <|
            \() ->
                "* [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is no gap between the title and the ']'" <|
            \() ->
                "- [X]foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is whitespace before the list marker" <|
            \() ->
                " - [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (\_ -> "failed")
                    |> Expect.equal (Err "failed")
        ]


toString : Test
toString =
    describe "toString"
        [ test "outputs an incomplete TaskList item with an empty checkbox" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [ ] foo")
        , test "outputs a completed TaskList item with a ticked checkbox" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] foo")
        , test "outputs a completed TaskList item with a (lower-case) ticked checkbox" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] foo")
        , test "removes excess whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] the task")
        , test "removed trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] the task")
        ]
