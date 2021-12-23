module TaskItemTests exposing (suite)

import Date
import Expect
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.TaskHelpers as TaskHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser exposing ((|=))
import TaskItem exposing (AutoCompletion(..), Completion(..), TaskItem)
import Test exposing (..)
import Time


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
        , id
        , isCompleted
        , isFromFile
        , isDated
        , notes
        , originalText
        , parsing
        , removeTag
        , subtasks
        , tags
        , tasksToToggle
        , title
        , toString
        , transformation
        , updateFilePath
        ]


autoComplete : Test
autoComplete =
    describe "autoComplete"
        [ test "returns TrueSpecified where the @autocomplete tag is true" <|
            \() ->
                "- [ ] foo @autocomplete(true)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.autoComplete
                    |> Expect.equal (Ok TrueSpecified)
        , test "returns NotSpecifed where there is no @autocomplete tag" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.autoComplete
                    |> Expect.equal (Ok NotSpecifed)
        , test "returns FalseSpecified where the @autocomplete tag is false" <|
            \() ->
                "- [ ] foo @autocomplete(false)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.autoComplete
                    |> Expect.equal (Ok FalseSpecified)
        , test "doesn't include the tag in the title" <|
            \() ->
                "- [ ] foo @autocomplete(false)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "includes an invalid tag in the title" <|
            \() ->
                "- [ ] foo @autocomplete(falsey)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @autocomplete(falsey)")
        ]


blockLink : Test
blockLink =
    describe "blockLink"
        [ test "does not include any ^blockLink text in the title" <|
            \() ->
                "- [ ] foo ^bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "includes any ^blockLink text in the title if it is NOT at the end of the line" <|
            \() ->
                "- [ ] foo ^bar baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo ^bar baz")
        ]


completion : Test
completion =
    describe "completion"
        [ test "returns Incomplete for an incomplete task" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns Complete for a completed task with no @completed() date" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        , test "returns Incomplete for an incomplete task with a @completed() date" <|
            \() ->
                "- [ ] foo @completed(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns CompletedAt for an completed task with a @completed() date" <|
            \() ->
                "- [x] foo @completed(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
        , test "returns Completed for an completed task with an invalid @completed() date" <|
            \() ->
                "- [x] foo @completed(2020-01-51)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        , test "the @completed() date is not included in the title" <|
            \() ->
                "- [x] foo @completed(2020-01-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the @completed() date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo @completed(2020-51-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @completed(2020-51-01) bar")
        ]


containsId : Test
containsId =
    describe "containsId"
        [ test "returns False if the id is not in the task or any subtasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "fileA" Nothing [])
                    |> Result.map (TaskItem.containsId (TaskHelpers.taskId "fileA" 4))
                    |> Expect.equal (Ok False)
        , test "returns True if the id is for the task" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "fileA" Nothing [])
                    |> Result.map (TaskItem.containsId (TaskHelpers.taskId "fileA" 1))
                    |> Expect.equal (Ok True)
        , test "returns True if the id is for one of the subtasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "fileA" Nothing [])
                    |> Result.map (TaskItem.containsId (TaskHelpers.taskId "fileA" 2))
                    |> Expect.equal (Ok True)
        ]


due : Test
due =
    describe "due"
        [ test "returns Nothing for a task with no file date or @due() date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the file date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" (Just "not a date") [])
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the @due date is invalid" <|
            \() ->
                "- [ ] foo @due(2020-51-01)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Just the date if the file date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" (Just "2020-01-07") [])
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737431)
        , test "@due() date over-rides the file date" <|
            \() ->
                "- [ ] foo @due(2021-03-03)"
                    |> Parser.run (TaskItem.parser "" (Just "2021-03-01") [])
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737852)
        , test "the @due() date is not included in the title" <|
            \() ->
                "- [x] foo @due(2020-01-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the @due() date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo @due(2020-51-01) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @due(2020-51-01) bar")
        , test "the @due() date is included in the title if it is followed by a non-space character" <|
            \() ->
                "- [x] foo @due(2020-51-01)x bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @due(2020-51-01)x bar")
        ]


filePath : Test
filePath =
    describe "filePath"
        [ test "returns the filePath" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing [])
                    |> Result.map TaskItem.filePath
                    |> Expect.equal (Ok "File A")
        ]


hasOneOfTheTags : Test
hasOneOfTheTags =
    describe "hasOneOfTheTags - basic operation"
        [ test "returns True if the task has one of the tags" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasOneOfTheTags [ "bar", "baz" ])
                    |> Expect.equal (Ok True)
        , test "returns False if the task has none of the tags" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasOneOfTheTags [ "qux", "baz" ])
                    |> Expect.equal (Ok False)
        ]


hasTags : Test
hasTags =
    describe "hasTags"
        [ test "returns True if the task has tags" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok True)
        , test "returns True if the task has subtasks with tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok True)
        , test "returns False if the task and subtasks have no tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok False)
        ]


hasTagBasic : Test
hasTagBasic =
    describe "hasTag - basic operation"
        [ test "returns True if the task has tags INCLUDING the given one" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "BAR")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has a subtask with the given tag" <|
            \() ->
                "- [ ] foo \n  - [ ] bar #bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok True)
        , test "returns False if the task has no tags" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has tags but NOT the given one" <|
            \() ->
                "- [ ] foo #baz #barrrrr #foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has a tags that starts with the given one" <|
            \() ->
                "- [ ] foo #bart"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the tag but it is followed by a slash" <|
            \() ->
                "- [ ] foo #bar/"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the tag but it is followed by a subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar")
                    |> Expect.equal (Ok False)
        ]


hasTagWithSubtag : Test
hasTagWithSubtag =
    describe "hasTag - with subtag"
        [ test "returns True if the task has the given subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/baz")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #bar/Baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "BAR/BAZ")
                    |> Expect.equal (Ok True)
        , test "returns False if the task has the subtag but it is followed by a slash" <|
            \() ->
                "- [ ] foo #bar/baz/"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/baz")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the subtag but it is followed by another subtag" <|
            \() ->
                "- [ ] foo #bar/baz/qux"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/baz")
                    |> Expect.equal (Ok False)
        , test "only matches actual subtags" <|
            \() ->
                "- [ ] foo #bart"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok False)
        ]


hasTagWithSubtagWildcard : Test
hasTagWithSubtagWildcard =
    describe "hasTag - with subtag wildcard"
        [ test "returns True if the task has a / on the end" <|
            \() ->
                "- [ ] foo #bar/"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #bAr/"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has a subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has nested subtags" <|
            \() ->
                "- [ ] foo #bar/baz/qux"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has no slash on the end" <|
            \() ->
                "- [ ] foo #bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.hasTag "bar/")
                    |> Expect.equal (Ok True)
        ]


id : Test
id =
    describe "id"
        [ test "returns FNVC1a(filePath):row" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing [])
                    |> Result.map TaskItem.id
                    |> Expect.equal (Ok "1414514984:1")
        ]


isCompleted : Test
isCompleted =
    describe "isCompleted"
        [ test "returns False if the checkbox is NOT checked" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok False)
        , test "returns True if the checkbox is checked with an x" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok True)
        , test "returns True if the checkbox is checked with an X" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok True)
        ]


isDated : Test
isDated =
    describe "isDated"
        [ test "returns False if there was no date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok False)
        , test "returns False if the date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" (Just "not a date") [])
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok False)
        , test "returns True if the date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" (Just "2020-01-07") [])
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok True)
        ]


isFromFile : Test
isFromFile =
    describe "isFromFile"
        [ test "returns False if the filenames do NOT match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing [])
                    |> Result.map (TaskItem.isFromFile "File B")
                    |> Expect.equal (Ok False)
        , test "returns True if the filenames do match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing [])
                    |> Result.map (TaskItem.isFromFile "File A")
                    |> Expect.equal (Ok True)
        , test "returns True if the filenames are both blank" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.isFromFile "")
                    |> Expect.equal (Ok True)
        , test "returns False if the filenames do NOT match case" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing [])
                    |> Result.map (TaskItem.isFromFile "File a")
                    |> Expect.equal (Ok False)
        , test "returns False if the filenames are a partial match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing [])
                    |> Result.map (TaskItem.isFromFile "File")
                    |> Expect.equal (Ok False)
        , test "matches id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "File A" Nothing [])
                    |> Result.map (TaskItem.isFromFile "File")
                    |> Expect.equal (Ok False)
        ]


notes : Test
notes =
    describe "notes"
        [ test "returns an empty string if there are no notes" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "")
        , test "returns non-task indented text as notes" <|
            \() ->
                -- ideally this shouldn't need to have a \n on the end!
                "- [ ] foo\n some notes\n"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "some notes")
        , test "returns multiple notes when intersperced with tasks and blank lines" <|
            \() ->
                -- ideally this shouldn't need to have a \n on the end!
                "- [ ] foo\n some notes\n  - [ ] a subtask\n\n  more notes\n  - [ ]invalid subtask\n"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "some notes\nmore notes\n- [ ]invalid subtask")
        ]


originalText : Test
originalText =
    describe "originalText"
        [ test "retains the original line text" <|
            \() ->
                "- [X]  the @due(2019-12-30) task @completed(2020-01-01) title "
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.originalText
                    |> Expect.equal (Ok "- [X]  the @due(2019-12-30) task @completed(2020-01-01) title ")
        , test "retains leading whitepace for the  original line text for subtasks" <|
            \() ->
                "- [X] task\n   \t - [ ] sub-task"
                    |> Parser.run (TaskItem.parser "" Nothing [])
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
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "consumes <eol> character" <|
            \() ->
                "- [X] foo\n- [ ] bar"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItem.parser "" Nothing []
                            |= TaskItem.parser "" Nothing []
                        )
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "fails to parse if not given a task" <|
            \() ->
                "- [X] foo"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItem.parser "" Nothing []
                            |= TaskItem.parser "" Nothing []
                        )
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task which ends straight after the ']'" <|
            \() ->
                "- [X]"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with no title" <|
            \() ->
                "- [X] "
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with a '*' as the list marker" <|
            \() ->
                "* [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is no gap between the title and the ']'" <|
            \() ->
                "- [X]foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is whitespace before the list marker" <|
            \() ->
                " - [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        ]


removeTag : Test
removeTag =
    describe "removeTag"
        [ test "removes tag when it's the only tag" <|
            \() ->
                "- [ ] foo #bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.removeTag "bar")
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo")
        , test "remove tag when there are other tags present" <|
            \() ->
                "- [ ] foo #bar #baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.removeTag "bar")
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo #baz")
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #bAr #baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.removeTag "Bar")
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo #baz")
        , test "remove tag when there is another tag that starts with the same" <|
            \() ->
                "- [ ] foo #bar #bart"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.removeTag "bar")
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo #bart")
        , test "remove tag when tag includes a slash" <|
            \() ->
                "- [ ] foo #bar/ #baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.removeTag "bar/")
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo #baz")
        , test "it will not remove sub-tags when a trailing slash is given" <|
            \() ->
                "- [ ] foo #bar/qux #baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.removeTag "bar/")
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo #bar/qux #baz")
        , test "remove tag when tag is on a subtask" <|
            \() ->
                "- [ ] foo #baz\n  - [ ] bar #bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.removeTag "bar")
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.toString)
                    |> Expect.equal (Ok [ "  - [ ] bar" ])
        ]


subtasks : Test
subtasks =
    describe "subtasks"
        [ test "parses subtasks" <|
            \() ->
                "- [ ] foo\n - [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "stops parsing subtasks at the end of indentation" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz\n- [ ] roo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar", "baz" ])
        , test "it is happy even if the level of indentation decreases as long as still indented" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n - [ ] baz\n- [ ] roo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.subtasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar", "baz" ])
        , test "consumes <eol> character where there are subtasks" <|
            \() ->
                "- [X] foo\n - [ ] sub foo\n- [ ] bar"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItem.parser "" Nothing []
                            |= TaskItem.parser "" Nothing []
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
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok [])
        , test "returns all tags from front matter, the top level, and sub tasks" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2\n  - [ ] bar #tag3"
                    |> Parser.run (TaskItem.parser "" Nothing [ "tagA", "tagB" ])
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok [ "tagA", "tagB", "tag1", "tag2", "tag3" ])
        , test "returns unique list of tags" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2\n  - [ ] bar #tag2"
                    |> Parser.run (TaskItem.parser "" Nothing [ "tag1" ])
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok [ "tag1", "tag2" ])
        , test "tags are not included in the title" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2"
                    |> Parser.run (TaskItem.parser "" Nothing [ "tag3" ])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        ]


tasksToToggle : Test
tasksToToggle =
    describe "tasksToToggle"
        [ test "returns an empty array if there are no tasks matching the id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 3) <| { now = Time.millisToPosix 0 })
                    |> Expect.equal (Ok [])
        , test "returns the TaskItem if it matches the id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 1) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "returns the sub-TaskItem if it matches the id and @autocomplete is not set" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the TaskItem and the sub-TaskItem if the subtask matches the id and @autocomplete is true and all other subtasks are complete" <|
            \() ->
                "- [ ] foo @autocomplete(true)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "returns the TaskItem and the sub-TaskItem if the subtask matches the id and @autocomplete is false and all other subtasks are complete" <|
            \() ->
                "- [ ] foo @autocomplete(false)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the sub-TaskItem if the subtask matches the id and @autocomplete is set but there are other incomplete subtasks" <|
            \() ->
                "- [ ] foo @autocomplete(true)\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the sub-TaskItem if the subtask matches the id, @autocomplete is set and the top level task is already completed" <|
            \() ->
                "- [x] foo @autocomplete(true)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the TaskItem if it is complete, matches and all subtasks are already complete" <|
            \() ->
                "- [x] foo @autocomplete(true)\n  - [x] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 1) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "returns the TaskItem if it is incomplete, matches and all subtasks are already complete" <|
            \() ->
                "- [ ] foo @autocomplete(true)\n  - [x] bar\n  - [x] baz"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 1) <| { now = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        ]


title : Test
title =
    describe "title"
        [ test "gets the title from a TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "handles lots of whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "the task")
        , test "looses trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "the task")
        ]


toString : Test
toString =
    describe "toString"
        [ test "outputs an incomplete TaskList item with an empty checkbox" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [ ] foo")
        , test "outputs a completed TaskList item with a ticked checkbox" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo")
        , test "outputs a completed TaskList item with a (lower-case) ticked checkbox" <|
            \() ->
                "- [X] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo")
        , test "outputs all tags except front matter tags" <|
            \() ->
                "- [X] #tag1 foo #tag2 bar #tag3"
                    |> Parser.run (TaskItem.parser "" Nothing [ "tag4" ])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar #tag1 #tag2 #tag3")
        , test "outputs a @completed(<iso-date>) TaskList item with the completed date at the end" <|
            \() ->
                "- [X] foo @completed(2020-03-22) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @completed(2020-03-22T00:00:00)")
        , test "outputs a @due(<iso-date>) TaskList item if the original had a @due tag" <|
            \() ->
                "- [X] foo @due(2020-03-22) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @due(2020-03-22)")
        , test "does not output a @due(<iso-date>) TaskList item if the original had a file based due date" <|
            \() ->
                "- [X] foo bar"
                    |> Parser.run (TaskItem.parser "" (Just "2021-03-01") [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar")
        , test "outputs an @autocomplete(true) TaskList item if in the original" <|
            \() ->
                "- [X] foo @autocomplete(true) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @autocomplete(true)")
        , test "outputs an @autocomplete(false) TaskList item if in the original" <|
            \() ->
                "- [X] foo @autocomplete(false) bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @autocomplete(false)")
        , test "does not output an @autocomplete() TaskList item if the original didn't specify one" <|
            \() ->
                "- [X] foo bar"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar")
        , test "outputs a blockLink at the end of the TaskList if in the original" <|
            \() ->
                "- [X] foo @autocomplete(false) bar ^1234"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] foo bar @autocomplete(false) ^1234")
        , test "removes excess whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] the task")
        , test "removes trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.toString
                    |> Expect.equal (Ok "- [x] the task")
        , test "preserves leading whitespace for subtasks" <|
            \() ->
                "- [X] the task\n   \t- [ ] a subtask"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map TaskItem.subtasks
                    |> Result.withDefault []
                    |> List.map TaskItem.toString
                    |> Expect.equal [ "   \t- [ ] a subtask" ]
        , test "does not output frontmatter tags for subtasks" <|
            \() ->
                "- [X] the task\n   \t- [ ] a subtask"
                    |> Parser.run (TaskItem.parser "" Nothing [ "aTag" ])
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
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.toggleCompletion <| { now = Time.millisToPosix 0 })
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok False)
        , test "toggling a task completed on a given date produces one marked as incomplete" <|
            \() ->
                "- [x] foo @completed(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.toggleCompletion <| { now = Time.millisToPosix 0 })
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok False)
        , test "toggling an incomplete task produces one marked as complete on the given date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.toggleCompletion { now = DateTimeHelpers.yearStart })
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt DateTimeHelpers.yearStart)
        , test "toggling a incomplete task with a @completed date updates the @completed date" <|
            \() ->
                "- [ ] foo @completed(2020-01-01)"
                    |> Parser.run (TaskItem.parser "" Nothing [])
                    |> Result.map (TaskItem.toggleCompletion { now = DateTimeHelpers.yearStart })
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt DateTimeHelpers.yearStart)
        ]


updateFilePath : Test
updateFilePath =
    describe "updateFilePath"
        [ test "sets the filePath if it didn't have one and the old path is blank" <|
            \() ->
                TaskItemHelpers.exampleTaskItem "- [x] foo" ""
                    |> TaskItem.updateFilePath "" "new/path"
                    |> TaskItem.filePath
                    |> Expect.equal "new/path"
        , test "updates an existing filePath" <|
            \() ->
                TaskItemHelpers.exampleTaskItem "- [x] foo" "old/path"
                    |> TaskItem.updateFilePath "old/path" "new/path"
                    |> TaskItem.filePath
                    |> Expect.equal "new/path"
        , test "does not update the filePath if the oldPath does not match" <|
            \() ->
                TaskItemHelpers.exampleTaskItem "- [x] foo" "old/path"
                    |> TaskItem.updateFilePath "other/path" "new/path"
                    |> TaskItem.filePath
                    |> Expect.equal "old/path"
        ]
