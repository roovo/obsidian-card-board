module TaskItemTests exposing (suite)

import Date exposing (Date)
import Expect
import Parser exposing ((|=))
import TaskItem exposing (AutoCompletion(..), Completion(..), Highlight(..))
import Test exposing (..)
import Time exposing (Month(..))


suite : Test
suite =
    concat
        [ autoComplete
        , blockLink
        , completion
        , containsId
        , due
        , filePath
        , hasOneOfTheTags
        , hasTags
        , hasTagBasic
        , hasTagWithSubtag
        , hasTagWithSubtagWildcard
        , highlight
        , id
        , inColumnId
        , isCompleted
        , isFromFile
        , isDated
        , notes
        , originalText
        , parsing
        , placeInColumn
        , subtasks
        , tags
        , tasksToToggle
        , title
        , toString
        , transformation
        ]


autoComplete : Test
autoComplete =
    describe "autoComplete"
        [ test "returns TrueSpecified where the @autodone tag is true" <|
            \() ->
                "- [ ] foo @autodone(true)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.autoComplete
                    |> Expect.equal (Ok TrueSpecified)
        , test "returns NotSpecifed where there is no @autodone tag" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.autoComplete
                    |> Expect.equal (Ok NotSpecifed)
        , test "returns FalseSpecified where the @autodone tag is false" <|
            \() ->
                "- [ ] foo @autodone(false)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.autoComplete
                    |> Expect.equal (Ok FalseSpecified)
        , test "doesn't include the tag in the title" <|
            \() ->
                "- [ ] foo @autodone(false)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "includes an invalid tag in the title" <|
            \() ->
                "- [ ] foo @autodone(falsey)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @autodone(falsey)")
        ]


blockLink : Test
blockLink =
    describe "blockLink"
        [ test "returns Nothing if there is no ^blockLink at the end of the line" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.blockLink
                    |> Expect.equal (Ok Nothing)
        , test "returns the ^blockLink text if it is at the end of the line" <|
            \() ->
                "- [ ] foo ^bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.blockLink
                    |> Expect.equal (Ok <| Just "^bar")
        , test "does not returns the ^blockLink text if it is NOT at the end of the line" <|
            \() ->
                "- [ ] foo ^bar baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.blockLink
                    |> Expect.equal (Ok Nothing)
        , test "does not include the ^blockLink text in the title" <|
            \() ->
                "- [ ] foo ^bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "includes the ^blockLink text in the title if it is NOT at the end of the line" <|
            \() ->
                "- [ ] foo ^bar baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo ^bar baz")
        ]


completion : Test
completion =
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
        , test "returns CompletedAt for an completed task with a @done() date" <|
            \() ->
                "- [x] foo @done(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
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


containsId : Test
containsId =
    describe "containsId"
        [ test "returns False if the id is not in the task or any subtasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "fileA" Nothing)
                    |> Result.map (TaskItem.containsId "fileA:4")
                    |> Expect.equal (Ok False)
        , test "returns True if the id is for the task" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "fileA" Nothing)
                    |> Result.map (TaskItem.containsId "fileA:1")
                    |> Expect.equal (Ok True)
        , test "returns True if the id is for one of the subtasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "fileA" Nothing)
                    |> Result.map (TaskItem.containsId "fileA:2")
                    |> Expect.equal (Ok True)
        ]


due : Test
due =
    describe "due"
        [ test "returns Nothing for a task with no file date or @due() date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the file date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "not a date")
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the @due date is invalid" <|
            \() ->
                "- [ ] foo @due(2020-51-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Just the date if the file date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "2020-01-07")
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737431)
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


filePath : Test
filePath =
    describe "filePath"
        [ test "returns the filePath" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map TaskItem.filePath
                    |> Expect.equal (Ok "File A")
        ]


hasOneOfTheTags : Test
hasOneOfTheTags =
    describe "hasOneOfTheTags - basic operation"
        [ test "returns True if the task has one of the tags" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasOneOfTheTags [ "bar", "baz" ])
                    |> Expect.equal (Ok True)
        , test "returns False if the task has none of the tags" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasOneOfTheTags [ "qux", "baz" ])
                    |> Expect.equal (Ok False)
        ]


hasTags : Test
hasTags =
    describe "hasTags"
        [ test "returns True if the task has tags" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok True)
        , test "returns True if the task has subtasks with tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok True)
        , test "returns False if the task and subtasks have no tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok False)
        ]


hasTagBasic : Test
hasTagBasic =
    describe "hasTag - basic operation"
        [ test "returns True if the task has tags INCLUDING the given one" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "BAR")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has a subtask with the given tag" <|
            \() ->
                "- [ ] foo \n  - [ ] bar #bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok True)
        , test "returns False if the task has no tags" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has tags but NOT the given one" <|
            \() ->
                "- [ ] foo #baz #barrrrr #foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has a tags that starts with the given one" <|
            \() ->
                "- [ ] foo #bart"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the tag but it is followed by a slash" <|
            \() ->
                "- [ ] foo #bar/"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the tag but it is followed by a subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        ]


hasTagWithSubtag : Test
hasTagWithSubtag =
    describe "hasTag - with subtag"
        [ test "returns True if the task has the given subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/baz")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #bar/Baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "BAR/BAZ")
                    |> Expect.equal (Ok True)
        , test "returns False if the task has the subtag but it is followed by a slash" <|
            \() ->
                "- [ ] foo #bar/baz/"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/baz")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the subtag but it is followed by another subtag" <|
            \() ->
                "- [ ] foo #bar/baz/qux"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/baz")
                    |> Expect.equal (Ok False)
        ]


hasTagWithSubtagWildcard : Test
hasTagWithSubtagWildcard =
    describe "hasTag - with subtag wildcard"
        [ test "returns True if the task has a / on the end" <|
            \() ->
                "- [ ] foo #bar/"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #bAr/"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has a subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has nested subtags" <|
            \() ->
                "- [ ] foo #bar/baz/qux"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has no slash on the end" <|
            \() ->
                "- [ ] foo #bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        ]


highlight : Test
highlight =
    describe "highlight"
        [ test "returns HighlightNone for a task with no due date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.highlight now Time.utc)
                    |> Expect.equal (Ok HighlightNone)
        , test "returns HighlightImportant for a task that is due today" <|
            \() ->
                "- [ ] foo @due(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.highlight now Time.utc)
                    |> Expect.equal (Ok HighlightImportant)
        , test "returns HighlightCritical for a task that is overdue" <|
            \() ->
                "- [ ] foo @due(2019-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.highlight now Time.utc)
                    |> Expect.equal (Ok HighlightCritical)
        ]


id : Test
id =
    describe "id"
        [ test "returns filePath:row" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map TaskItem.id
                    |> Expect.equal (Ok "File A:1")
        ]


inColumnId : Test
inColumnId =
    describe "inColumnId"
        [ test "returns filePath:row if the task has not been placed in a column" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map TaskItem.inColumnId
                    |> Expect.equal (Ok "File A:1")
        , test "returns columnName:filePath:row if the task has been placed in a column" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (TaskItem.placeInColumn "c1")
                    |> Result.map TaskItem.inColumnId
                    |> Expect.equal (Ok "c1:File A:1")
        ]


isCompleted : Test
isCompleted =
    describe "isCompleted"
        [ test "returns False if the checkbox is NOT checked" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok False)
        , test "returns True if the checkbox is checked with an x" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok True)
        , test "returns True if the checkbox is checked with an X" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok True)
        ]


isDated : Test
isDated =
    describe "isDated"
        [ test "returns False if there was no date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok False)
        , test "returns False if the date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "not a date")
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok False)
        , test "returns True if the date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" <| Just "2020-01-07")
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok True)
        ]


isFromFile : Test
isFromFile =
    describe "isFromFile"
        [ test "returns False if the filenames do NOT match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (TaskItem.isFromFile "File B")
                    |> Expect.equal (Ok False)
        , test "returns True if the filenames do match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (TaskItem.isFromFile "File A")
                    |> Expect.equal (Ok True)
        , test "returns True if the filenames are both blank" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.isFromFile "")
                    |> Expect.equal (Ok True)
        , test "returns False if the filenames do NOT match case" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (TaskItem.isFromFile "File a")
                    |> Expect.equal (Ok False)
        , test "returns False if the filenames are a partial match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (TaskItem.isFromFile "File")
                    |> Expect.equal (Ok False)
        , test "matches id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing)
                    |> Result.map (TaskItem.isFromFile "File")
                    |> Expect.equal (Ok False)
        ]


notes : Test
notes =
    describe "notes"
        [ test "returns an empty string if there are no notes" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "")
        , test "returns non-task indented text as notes" <|
            \() ->
                -- ideally this shouldn't need to have a \n on the end!
                "- [ ] foo\n some notes\n"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "some notes")
        , test "returns multiple notes when intersperced with tasks and blank lines" <|
            \() ->
                -- ideally this shouldn't need to have a \n on the end!
                "- [ ] foo\n some notes\n  - [ ] a subtask\n\n  more notes\n  - [ ]invalid subtask\n"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "some notes\nmore notes\n- [ ]invalid subtask")
        ]


originalText : Test
originalText =
    describe "originalText"
        [ test "retains the original line text" <|
            \() ->
                "- [X]  the @due(2019-12-30) task @done(2020-01-01) title "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.originalText
                    |> Expect.equal (Ok "- [X]  the @due(2019-12-30) task @done(2020-01-01) title ")
        , test "retains leading whitepace for the  original line text for subtasks" <|
            \() ->
                "- [X] task\n   \t - [ ] sub-task"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.originalText)
                    |> Expect.equal (Ok [ "   \t - [ ] sub-task" ])
        ]


parsing : Test
parsing =
    describe "parsing"
        [ test "only looks at the first line of a multline string" <|
            \() ->
                "- [X] foo\n- [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
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
        , test "fails to parse if not given a task" <|
            \() ->
                "- [X] foo"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItem.parser "" Nothing
                            |= TaskItem.parser "" Nothing
                        )
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task which ends straight after the ']'" <|
            \() ->
                "- [X]"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with no title" <|
            \() ->
                "- [X] "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with a '*' as the list marker" <|
            \() ->
                "* [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is no gap between the title and the ']'" <|
            \() ->
                "- [X]foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is whitespace before the list marker" <|
            \() ->
                " - [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        ]


placeInColumn : Test
placeInColumn =
    describe "placeInColumn"
        [ test "does not add the column title to the start of the id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "fa" Nothing)
                    |> Result.map (TaskItem.placeInColumn "c1")
                    |> Result.map TaskItem.id
                    |> Expect.equal (Ok "fa:1")
        , test "adds the column title to the start of the inColumnId" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "fa" Nothing)
                    |> Result.map (TaskItem.placeInColumn "c1")
                    |> Result.map TaskItem.inColumnId
                    |> Expect.equal (Ok "c1:fa:1")
        ]


subtasks : Test
subtasks =
    describe "subtasks"
        [ test "parses subtasks" <|
            \() ->
                "- [ ] foo\n - [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "stops parsing subtasks at the end of indentation" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz\n- [ ] roo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar", "baz" ])
        , test "it is happy even if the level of indentation decreases as long as still indented" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n - [ ] baz\n- [ ] roo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.title)
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
        , test "returns all tags from the top level and sub tasks" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2\n  - [ ] bar #tag3"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok [ "tag1", "tag2", "tag3" ])
        , test "tags are not included in the title" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        ]


tasksToToggle : Test
tasksToToggle =
    describe "tasksToToggle"
        [ test "returns an empty array if there are no tasks matching the id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":3" <| Time.millisToPosix 0)
                    |> Expect.equal (Ok [])
        , test "returns the TaskItem if it matches the id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":1" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "returns the sub-TaskItem if it matches the id and @autodone is not set" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":2" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the TaskItem and the sub-TaskItem if the subtask matches the id and @autodone is true and all other subtasks are complete" <|
            \() ->
                "- [ ] foo @autodone(true)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":2" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "returns the TaskItem and the sub-TaskItem if the subtask matches the id and @autodone is false and all other subtasks are complete" <|
            \() ->
                "- [ ] foo @autodone(false)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":2" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the sub-TaskItem if the subtask matches the id and @autodone is set but there are other incomplete subtasks" <|
            \() ->
                "- [ ] foo @autodone(true)\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":2" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the sub-TaskItem if the subtask matches the id, @autodone is set and the top level task is already completed" <|
            \() ->
                "- [x] foo @autodone(true)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":2" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the TaskItem if it is complete, matches and all subtasks are already complete" <|
            \() ->
                "- [x] foo @autodone(true)\n  - [x] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":1" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "returns the TaskItem if it is incomplete, matches and all subtasks are already complete" <|
            \() ->
                "- [ ] foo @autodone(true)\n  - [x] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.tasksToToggle ":1" <| Time.millisToPosix 0)
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        ]


title : Test
title =
    describe "title"
        [ test "gets the title from a TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "handles lots of whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "the task")
        , test "looses trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "the task")
        ]


toString : Test
toString =
    describe "toString"
        [ test "outputs an incomplete TaskList item with an empty checkbox" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo")
        , test "outputs a completed TaskList item with a ticked checkbox" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo")
        , test "outputs a completed TaskList item with a (lower-case) ticked checkbox" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo")
        , test "outputs any tags" <|
            \() ->
                "- [X] #tag1 foo #tag2 bar #tag3"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar #tag1 #tag2 #tag3")
        , test "outputs a @done(<iso-date>) TaskList item with the done date at the end" <|
            \() ->
                "- [X] foo @done(2020-03-22) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @done(2020-03-22T00:00:00)")
        , test "outputs a @due(<iso-date>) TaskList item if the original had a @due tag" <|
            \() ->
                "- [X] foo @due(2020-03-22) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @due(2020-03-22)")
        , test "does not output a @due(<iso-date>) TaskList item if the original had a file based due date" <|
            \() ->
                "- [X] foo bar"
                    |> Parser.run (TaskItem.parser "" <| Just "2021-03-01")
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar")
        , test "outputs an @autodone(true) TaskList item if in the original" <|
            \() ->
                "- [X] foo @autodone(true) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @autodone(true)")
        , test "outputs an @autodone(false) TaskList item if in the original" <|
            \() ->
                "- [X] foo @autodone(false) bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @autodone(false)")
        , test "does not output an @autodone() TaskList item if the original didn't specify one" <|
            \() ->
                "- [X] foo bar"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar")
        , test "outputs a blockLink at the end of the TaskList if in the original" <|
            \() ->
                "- [X] foo @autodone(false) bar ^1234"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @autodone(false) ^1234")
        , test "removes excess whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] the task")
        , test "removes trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] the task")
        , test "preserves leading whitespace for subtasks" <|
            \() ->
                "- [X] the task\n   \t- [ ] a subtask"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map TaskItem.subtasks
                    |> Result.withDefault []
                    |> List.map TaskItem.toString
                    |> Expect.equal [ "   \t- [ ] a subtask" ]
        ]


transformation : Test
transformation =
    describe "transformation"
        [ test "toggling a completed task produces one marked as incomplete" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.toggleCompletion <| Time.millisToPosix 0)
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok False)
        , test "toggling a task completed on a given date produces one marked as incomplete" <|
            \() ->
                "- [x] foo @done(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.toggleCompletion <| Time.millisToPosix 0)
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok False)
        , test "toggling an incomplete task produces one marked as complete on the given date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.toggleCompletion now)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt now)
        , test "toggling a incomplete task with a @done date updates the @done date" <|
            \() ->
                "- [ ] foo @done(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (TaskItem.toggleCompletion now)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt now)
        ]



-- HELPERS


now : Time.Posix
now =
    -- 2020-01-01
    Time.millisToPosix 1577836800000
