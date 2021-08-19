module TaskItemTests exposing (suite)

import Date exposing (Date)
import Expect exposing (Expectation)
import Parser exposing ((|=))
import TaskItem exposing (Completion(..), TaskItem)
import TaskList exposing (TaskList)
import Test exposing (..)
import Time exposing (Month(..))


suite : Test
suite =
    concat
        [ done
        , due
        , info
        , parsing
        , tags
        , toString
        , transformation
        ]


tags : Test
tags =
    describe "tags"
        [ test "returns an empty array if there are no tags" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok [])
        , test "returns any specified tags" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok [ "tag1", "tag2" ])
        , test "tags are not included in the title" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        ]


due : Test
due =
    describe "due"
        [ test "returns undated for a task with no file date or @due() date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "@due() date over-rides the file date" <|
            \() ->
                "- [ ] foo @due(2021-03-03)"
                    |> Parser.run (TaskItem.parser "" <| Just "2021-03-01")
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737852)
        , test "the @due() date is not included in the title" <|
            \() ->
                "- [x] foo @due(2020-01-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the @due() date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo @due(2020-51-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @due(2020-51-01) bar")
        , test "the @due() date is included in the title if it is followed by a non-space character" <|
            \() ->
                "- [x] foo @due(2020-51-01)x bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @due(2020-51-01)x bar")
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
        , test "returns Incomplete for an incomplete task with a @done() date" <|
            \() ->
                "- [ ] foo @done(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns CompletedOn for an completed task with a @done() date" <|
            \() ->
                "- [x] foo @done(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedOn <| Date.fromCalendarDate 2020 Jan 1)
        , test "returns Completed for an completed task with an invalid @done() date" <|
            \() ->
                "- [x] foo @done(2020-01-51)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        , test "the @done() date is not included in the title" <|
            \() ->
                "- [x] foo @done(2020-01-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the @done() date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo @done(2020-51-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @done(2020-51-01) bar")
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
        , test "matches id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (\t -> TaskItem.isFromFile "File" t)
                    |> Expect.equal (Ok False)
        ]


transformation : Test
transformation =
    describe "transformation"
        [ test "toggling a completed task produces one marked as incomplete" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toggleCompletion Nothing t)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok False)
        , test "toggling a task completed on a given date produces one marked as incomplete" <|
            \() ->
                "- [x] foo @done(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toggleCompletion Nothing t)
                    |> Result.map (\t -> TaskItem.isCompleted t)
                    |> Expect.equal (Ok False)
        , test "giving no date when toggling an incomplete task produces one marked as complete" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toggleCompletion Nothing t)
                    |> Result.map (\t -> TaskItem.completion t)
                    |> Expect.equal (Ok Completed)
        , test "giving date when toggling an incomplete task produces one marked as complete on that date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toggleCompletion (Just today) t)
                    |> Result.map (\t -> TaskItem.completion t)
                    |> Expect.equal (Ok <| CompletedOn today)
        , test "toggling a incomplete task with a @done date updates the @done date" <|
            \() ->
                "- [ ] foo @done(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toggleCompletion (Just today) t)
                    |> Result.map (\t -> TaskItem.completion t)
                    |> Expect.equal (Ok <| CompletedOn today)
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
        , test "looses trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "the task")
        , test "retains the original line text" <|
            \() ->
                "- [X]  the @due(2019-12-30) task @done(2020-01-01) title\n- [ ] another task  "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.originalText t)
                    |> Expect.equal (Ok "- [X]  the @due(2019-12-30) task @done(2020-01-01) title")
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
        , test "parses subtasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\ta -> TaskItem.subtasks ta)
                    |> Result.map (\ta -> List.map (\tb -> TaskItem.title tb) ta)
                    |> Expect.equal (Ok [ "bar" ])
        , test "stops parsing subtasks at the end of indentation" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz\n- [ ] roo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\ta -> TaskItem.subtasks ta)
                    |> Result.map (\ta -> List.map (\tb -> TaskItem.title tb) ta)
                    |> Expect.equal (Ok [ "bar", "baz" ])
        , test "it is happy even if the level of indentation decreases as long as still indented" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n - [ ] baz\n- [ ] roo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\ta -> TaskItem.subtasks ta)
                    |> Result.map (\ta -> List.map (\tb -> TaskItem.title tb) ta)
                    |> Expect.equal (Ok [ "bar", "baz" ])
        , test "consumes <eol> character where there are subtasks" <|
            \() ->
                "- [X] foo\n - [ ] sub foo\n- [ ] bar"
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
        , test "outputs a @done(<iso-date>) TaskList item with the done date at the end" <|
            \() ->
                "- [X] foo @done(2020-03-22) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] foo bar @done(2020-03-22)")
        , test "outputs a @due(<iso-date>) TaskList item if the original had a @due tag" <|
            \() ->
                "- [X] foo @due(2020-03-22) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] foo bar @due(2020-03-22)")
        , test "does not output a @due(<iso-date>) TaskList item if the original had a file based due date" <|
            \() ->
                "- [X] foo bar"
                    |> Parser.run (TaskItem.parser "" <| Just "2021-03-01")
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] foo bar")
        , test "removes excess whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] the task")
        , test "removes trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.toString t)
                    |> Expect.equal (Ok "- [x] the task")
        ]


today : Date
today =
    Date.fromCalendarDate 2000 Oct 20
