module TaskItemTests exposing (suite)

import DataviewTaskCompletion
import Date
import Expect
import GlobalSettings
import Helpers.TaskHelpers as TaskHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser exposing ((|=))
import TagList
import TaskItem exposing (Completion(..))
import Test exposing (..)
import Time


suite : Test
suite =
    concat
        [ allSubtasksWithMatchingTagCompleted
        , blockLink
        , completedPosix
        , completion
        , containsId
        , descendantTaskHasThisTag
        , descendantTasks
        , due
        , filePath
        , hasAnyIncompleteSubtasksWithTagsOtherThanThese
        , hasIncompleteTaskWithThisTag
        , hasOneOfTheTags
        , hasTags
        , hasTaskWithTagOtherThanThese
        , hasThisTagBasic
        , hasThisTagWithSubtag
        , hasThisTagWithSubtagWildcard
        , hasTopLevelTags
        , id
        , isCompleted
        , isFromFile
        , isDated
        , lineNumber
        , notes
        , originalText
        , parsing
        , removeTags
        , tags
        , tasksToToggle
        , title
        , titleWithTags
        , topLevelTags
        , topLevelTaskHasThisTag
        , toToggledString
        , updateFilePath
        ]


allSubtasksWithMatchingTagCompleted : Test
allSubtasksWithMatchingTagCompleted =
    describe "allSubtasksWithMatchingTagCompleted"
        [ test "returns False for an incompleted task with no sub-tasks" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "bar")
                    |> Expect.equal (Ok False)
        , test "returns False for a completed task with no sub-tasks" <|
            \() ->
                "- [x] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "bar")
                    |> Expect.equal (Ok False)
        , test "returns False for an incompleted task with incompleted sub-tasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "bar")
                    |> Expect.equal (Ok False)
        , test "returns False for a completed task with incompleted sub-tasks" <|
            \() ->
                "- [x] foo\n  - [ ] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "bar")
                    |> Expect.equal (Ok False)
        , test "returns False for a completed task with completed sub-tasks" <|
            \() ->
                "- [x] foo\n  - [x] bar\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "bar")
                    |> Expect.equal (Ok False)
        , test "returns True for a completed task with a completed sub-task with a matching tag" <|
            \() ->
                "- [x] foo\n  - [x] bar #plop\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "plop")
                    |> Expect.equal (Ok True)
        , test "returns False for a completed task with an incomplete sub-task with a matching tag and another the same but completed" <|
            \() ->
                "- [x] foo\n  - [ ] bar #plop\n  - [x] baz #plop"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "plop")
                    |> Expect.equal (Ok False)
        , test "returns False for a completed task with an incomplete sub-task with a matching sub-tag" <|
            \() ->
                "- [x] foo\n  - [ ] bar #plop/ploppy\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.allSubtasksWithMatchingTagCompleted "plop/")
                    |> Expect.equal (Ok False)
        ]


blockLink : Test
blockLink =
    describe "blockLink"
        [ test "does not include any ^blockLink text in the title" <|
            \() ->
                "- [ ] foo ^bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "includes any ^blockLink text in the title if it is NOT at the end of the line" <|
            \() ->
                "- [ ] foo ^bar baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo ^bar baz")
        ]


completedPosix : Test
completedPosix =
    describe "completedPosix"
        [ test "returns the completed time for a completed top level task if present" <|
            \() ->
                "- [x] foo @completed(2023-01-01T00:00:00)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completedPosix
                    |> Expect.equal (Ok 1672531200000)
        , test "returns 0 for a completed top level task if no @completed tag" <|
            \() ->
                "- [x] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completedPosix
                    |> Expect.equal (Ok 0)
        , test "returns 0 for an incomplete top level task" <|
            \() ->
                "- [ ] foo @completed(2023-01-01T00:00:00)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completedPosix
                    |> Expect.equal (Ok 0)
        , test "returns the completed time for a completed subtask level task if present" <|
            \() ->
                "- [ ] foo\n  - [x] bar @completed(2023-01-01T00:00:00)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completedPosix
                    |> Expect.equal (Ok 1672531200000)
        , test "the completed time for the top level task takes precedence" <|
            \() ->
                "- [x] foo @completed(2023-01-01T00:00:00)\n  - [x] bar @completed(2022-01-01T00:00:00)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completedPosix
                    |> Expect.equal (Ok 1672531200000)
        ]


completion : Test
completion =
    describe "completion"
        [ test "returns Incomplete for an incomplete task" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns Incomplete for an incomplete task marked with an asterisk" <|
            \() ->
                "* [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns Complete for a completed task with no inline completion date" <|
            \() ->
                "- [x] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        , test "returns Incomplete for an incomplete task with a @completed() date" <|
            \() ->
                "- [ ] foo @completed(2020-01-01)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns Incomplete for an incomplete task with a ✅ date" <|
            \() ->
                "- [ ] foo ✅ 2020-01-01"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns Incomplete for an incomplete task with a [completion:: date" <|
            \() ->
                "- [ ] foo [completion:: 2020-01-01]"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Incomplete)
        , test "returns CompletedAt for a completed task with a @completed() date" <|
            \() ->
                "- [x] foo @completed(2020-01-01)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
        , test "returns CompletedAt for a completed task with a ✅ date" <|
            \() ->
                "- [x] foo ✅ 2020-01-01"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
        , test "returns CompletedAt for a completed task with a [completion:: date" <|
            \() ->
                "- [x] foo [completion:: 2020-01-01]"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
        , test "returns CompletedAt for a completed task with a [custom:: date" <|
            \() ->
                "- [x] foo [custom:: 2020-01-01]"
                    |> Parser.run (TaskItem.parser (DataviewTaskCompletion.Text "custom") "" Nothing TagList.empty 0)
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
        , test "returns CompletedAt for a completed task with a 📅 date before the ✅ date" <|
            \() ->
                "- [x] foo 📅 2020-01-02 ✅ 2020-01-01"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
        , test "returns CompletedAt for a completed task with a ✅ date before the 📅 date" <|
            \() ->
                "- [x] foo ✅ 2020-01-01 📅 2020-01-01"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok <| CompletedAt <| Time.millisToPosix 1577836800000)
        , test "returns Completed for a completed task with an invalid @completed() date" <|
            \() ->
                "- [x] foo @completed(2020-01-51)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        , test "returns Completed for a completed task with an invalid [completion:: date" <|
            \() ->
                "- [x] foo [completion:: 2020-01-51]"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        , test "returns Completed for a completed task with an invalid ✅ date" <|
            \() ->
                "- [x] foo ✅ 2020-01-51"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.completion
                    |> Expect.equal (Ok Completed)
        , test "the @completed() date is not included in the title" <|
            \() ->
                "- [x] foo @completed(2020-01-01) bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the ✅ date is not included in the title" <|
            \() ->
                "- [x] foo ✅ 2020-01-01 bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the [completion:: date is not included in the title" <|
            \() ->
                "- [x] foo [completion:: 2020-01-01] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the @completed() date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo @completed(2020-51-01) bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @completed(2020-51-01) bar")
        , test "the ✅ date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo ✅ 2020-51-01 bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo ✅ 2020-51-01 bar")
        , test "the [completion:: date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo [completion:: 2020-51-01] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo [completion:: 2020-51-01] bar")
        ]


containsId : Test
containsId =
    describe "containsId"
        [ test "returns False if the id is not in the task or any descendant tasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "fileA" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.containsId (TaskHelpers.taskId "fileA" 4))
                    |> Expect.equal (Ok False)
        , test "returns True if the id is for the task" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "fileA" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.containsId (TaskHelpers.taskId "fileA" 1))
                    |> Expect.equal (Ok True)
        , test "returns True if the id is for one of the descendant tasks" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "fileA" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.containsId (TaskHelpers.taskId "fileA" 2))
                    |> Expect.equal (Ok True)
        ]


descendantTaskHasThisTag : Test
descendantTaskHasThisTag =
    describe "descendantTaskHasThisTag"
        [ test "returns False for (theTag) '- [ ] foo" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.descendantTaskHasThisTag "theTag")
                    |> Expect.equal (Ok False)
        , test "returns False for (theTag) '- [ ] foo #theTag'" <|
            \() ->
                "- [ ] foo #theTag"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.descendantTaskHasThisTag "theTag")
                    |> Expect.equal (Ok False)
        , test "returns False for (theTag) '- [ ] foo\n  - [ ] bar #otherTag'" <|
            \() ->
                "- [ ] foo\n  - [ ] bar #otherTag"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.descendantTaskHasThisTag "theTag")
                    |> Expect.equal (Ok False)
        , test "returns True for (theTag) '- [ ] foo\n  - [ ] bar #thisTag'" <|
            \() ->
                "- [ ] foo\n  - [ ] bar #theTag"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.descendantTaskHasThisTag "theTag")
                    |> Expect.equal (Ok True)
        ]


descendantTasks : Test
descendantTasks =
    describe "descendantTasks"
        [ test "parses descendantTasks" <|
            \() ->
                "- [ ] foo\n - [ ] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.descendantTasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "parses descendantTasks with a different list marker" <|
            \() ->
                "- [ ] foo\n + [ ] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.descendantTasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "stops parsing descendantTasks at the end of indentation" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n  - [ ] baz\n- [ ] roo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.descendantTasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar", "baz" ])
        , test "it is happy even if the level of indentation decreases as long as still indented" <|
            \() ->
                "- [ ] foo\n  - [ ] bar\n - [ ] baz\n- [ ] roo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.descendantTasks
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar", "baz" ])
        , test "consumes <eol> character where there are descendantTasks" <|
            \() ->
                "- [X] foo\n - [ ] sub foo\n- [ ] bar"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItemHelpers.basicParser
                            |= TaskItemHelpers.basicParser
                        )
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo", "bar" ])
        ]


due : Test
due =
    describe "due"
        [ test "returns Nothing for a task with no file date or inline due date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the file date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "not a date") TagList.empty 0)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the @due date is invalid" <|
            \() ->
                "- [ ] foo @due(2020-51-01)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the 📅 date is invalid" <|
            \() ->
                "- [ ] foo 📅 2020-51-01"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Nothing if the [due:: date is invalid" <|
            \() ->
                "- [ ] foo [due:: 2020-51-01]"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok Nothing)
        , test "returns Just the date if the file date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "2020-01-07") TagList.empty 0)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737431)
        , test "@due() date over-rides the file date" <|
            \() ->
                "- [ ] foo @due(2021-03-03)"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "2021-03-01") TagList.empty 0)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737852)
        , test "📅 date over-rides the file date" <|
            \() ->
                "- [ ] foo 📅 2021-03-03"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "2021-03-01") TagList.empty 0)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737852)
        , test "[due:: date over-rides the file date" <|
            \() ->
                "- [ ] foo [due:: 2021-03-03]"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "2021-03-01") TagList.empty 0)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737852)
        , test "📅 date over-rides the file date when preceeded by a ✅ date" <|
            \() ->
                "- [ ] foo ✅ 2021-03-02 📅 2021-03-03"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "2021-03-01") TagList.empty 0)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737852)
        , test "📅 date over-rides the file date when postceeded by a ✅ date" <|
            \() ->
                "- [ ] foo 📅 2021-03-03 ✅ 2021-03-02"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "2021-03-01") TagList.empty 0)
                    |> Result.map TaskItem.due
                    |> Expect.equal (Ok <| Just <| Date.fromRataDie 737852)
        , test "the @due() date is not included in the title" <|
            \() ->
                "- [x] foo @due(2020-01-01) bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the 📅 date is not included in the title" <|
            \() ->
                "- [x] foo 📅 2020-01-01 bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the [due:: date is not included in the title" <|
            \() ->
                "- [x] foo [due:: 2020-01-01] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        , test "the @due() date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo @due(2020-51-01) bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @due(2020-51-01) bar")
        , test "the 📅 date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo 📅 2020-51-01 bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo 📅 2020-51-01 bar")
        , test "the [due:: date is included in the title if it is not valid" <|
            \() ->
                "- [x] foo [due:: 2020-51-01] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo [due:: 2020-51-01] bar")
        , test "the @due() date is included in the title if it is followed by a non-space character" <|
            \() ->
                "- [x] foo @due(2020-51-01)x bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo @due(2020-51-01)x bar")
        ]


filePath : Test
filePath =
    describe "filePath"
        [ test "returns the filePath" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "File A" Nothing TagList.empty 0)
                    |> Result.map TaskItem.filePath
                    |> Expect.equal (Ok "File A")
        ]


hasAnyIncompleteSubtasksWithTagsOtherThanThese : Test
hasAnyIncompleteSubtasksWithTagsOtherThanThese =
    describe "hasAnyIncompleteSubtasksWithTagsOtherThanThese"
        [ describe "incomplete task"
            [ test "returns False for a task with no sub-tasks that has another tag" <|
                \() ->
                    "- [ ] foo #atag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns False for a task with an incomplete sub-task with no tag" <|
                \() ->
                    "- [ ] foo #atag\n  - [ ] bar"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns False for a task with an incomplete sub-task with one of the given tags" <|
                \() ->
                    "- [ ] foo #atag\n  - [ ] bar #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns True for a task with an incomplete sub-task with a different tag" <|
                \() ->
                    "- [ ] foo #atag\n  - [ ] bar #atag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns True for a task with an incomplete sub-task with a different tag and another that matches" <|
                \() ->
                    "- [ ] foo #atag\n  - [ ] bar #atag  - [ ] baz #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns True for a task with an incomplete sub-task with a different tag and another that is complete and matches" <|
                \() ->
                    "- [ ] foo #atag\n  - [ ] bar #atag  - [x] baz #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns False for a task with a completed sub-task with a different tag and another that is not complete and matches" <|
                \() ->
                    "- [ ] foo #atag\n  - [x] bar #atag  - [ ] baz #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            ]
        , describe "completed task"
            [ test "returns False for a task with no sub-tasks that has another tag" <|
                \() ->
                    "- [x] foo #atag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns False for a task with an incomplete sub-task with no tag" <|
                \() ->
                    "- [x] foo #atag\n  - [ ] bar"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns False for a task with an incomplete sub-task with one of the given tags" <|
                \() ->
                    "- [x] foo #atag\n  - [ ] bar #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns True for a task with an incomplete sub-task with a different tag" <|
                \() ->
                    "- [x] foo #atag\n  - [ ] bar #atag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns True for a task with an incomplete sub-task with a different tag and another that matches" <|
                \() ->
                    "- [x] foo #atag\n  - [ ] bar #atag  - [ ] baz #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns True for a task with an incomplete sub-task with a different tag and another that is complete and matches" <|
                \() ->
                    "- [x] foo #atag\n  - [ ] bar #atag  - [x] baz #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns False for a task with a completed sub-task with a different tag and another that is not complete and matches" <|
                \() ->
                    "- [x] foo #atag\n  - [x] bar #atag  - [ ] baz #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese [ "ztag" ])
                        |> Expect.equal (Ok False)
            ]
        ]


hasIncompleteTaskWithThisTag : Test
hasIncompleteTaskWithThisTag =
    describe "hasIncompleteTaskWithTag - basic operation"
        [ test "returns True for a task with no sub-tasks that has the tag" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasIncompleteTaskWithThisTag "bar")
                    |> Expect.equal (Ok True)
        , test "returns True for a task without the tag with no sub-tasks, but the tag is in the front matter" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "bar" ]) 0)
                    |> Result.map (TaskItem.hasIncompleteTaskWithThisTag "bar")
                    |> Expect.equal (Ok True)
        , test "returns True for a task with no matching tag but a sub-tasks has the tag" <|
            \() ->
                "- [ ] foo #foo\n  - [ ] bar #bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasIncompleteTaskWithThisTag "bar")
                    |> Expect.equal (Ok True)
        , test "returns False for a task with no sub-tasks that doesn't have the tag" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasIncompleteTaskWithThisTag "baz")
                    |> Expect.equal (Ok False)
        , test "returns False for a task with no sub-tasks that has the tag but is completed" <|
            \() ->
                "- [x] foo #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasIncompleteTaskWithThisTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False for a completed task with no sub-tasks without the tag, but the tag is in the front matter" <|
            \() ->
                "- [x] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "bar" ]) 0)
                    |> Result.map (TaskItem.hasIncompleteTaskWithThisTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False for a task with no matching tag plus a sub-tasks has the tag but is completed" <|
            \() ->
                "- [ ] foo #foo\n  - [x] bar #bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasIncompleteTaskWithThisTag "bar")
                    |> Expect.equal (Ok False)
        ]


hasOneOfTheTags : Test
hasOneOfTheTags =
    describe "hasOneOfTheTags - basic operation"
        [ test "returns True if the task has one of the tags" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasOneOfTheTags [ "bar", "baz" ])
                    |> Expect.equal (Ok True)
        , test "returns False if the task has none of the tags" <|
            \() ->
                "- [ ] foo #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasOneOfTheTags [ "qux", "baz" ])
                    |> Expect.equal (Ok False)
        ]


hasTags : Test
hasTags =
    describe "hasTags"
        [ test "returns True if the task has tags" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok True)
        , test "returns True if the task has any descendant tasks with tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar #baz #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok True)
        , test "returns True if the task has only front matter tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "tagA" ]) 0)
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok True)
        , test "returns False if the task and all descendant tasks have no tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.hasTags
                    |> Expect.equal (Ok False)
        ]


hasTaskWithTagOtherThanThese : Test
hasTaskWithTagOtherThanThese =
    describe "hasTaskWithTagOtherThanThese"
        [ describe "with no sub-tasks"
            [ test "returns True for a task which has a tag, but none of those given" <|
                \() ->
                    "- [ ] foo #aTag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns True for a task which has a tag, in addition to those given" <|
                \() ->
                    "- [ ] foo #aTag #yTag #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns False for a task which has no tags when none are given either" <|
                \() ->
                    "- [ ] foo"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [])
                        |> Expect.equal (Ok False)
            , test "returns False for a task which has no tags" <|
                \() ->
                    "- [ ] foo"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns False for a task which has no tags other than one of those given" <|
                \() ->
                    "- [ ] foo #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns False for a task which has exaclty the same as the given tags" <|
                \() ->
                    "- [ ] foo #ytag #ztag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok False)
            ]
        , describe "with sub-tasks"
            [ test "returns False where the task and sub-tasks have no tags" <|
                \() ->
                    "- [ ] foo\n  - [ ] bar"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok False)
            , test "returns False where the task and sub-tasks have no tags and none are given" <|
                \() ->
                    "- [ ] foo\n  - [ ] bar"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [])
                        |> Expect.equal (Ok False)
            , test "returns True where the sub-task has a tag other than those given" <|
                \() ->
                    "- [ ] foo\n  - [ ] bar #atag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns True where the sub-task has a tag other than those given even if the main task has a matching tag" <|
                \() ->
                    "- [ ] foo #ztag\n  - [ ] bar #atag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok True)
            , test "returns False where the sub- and main task have matching tags" <|
                \() ->
                    "- [ ] foo #ztag\n  - [ ] bar #ytag"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map (TaskItem.hasTaskWithTagOtherThanThese [ "ytag", "ztag" ])
                        |> Expect.equal (Ok False)
            ]
        ]


hasThisTagBasic : Test
hasThisTagBasic =
    describe "hasThisTag - basic operation"
        [ test "returns True if the task has tags INCLUDING the given one" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "BAR")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has a subtask with the given tag" <|
            \() ->
                "- [ ] foo \n  - [ ] bar #bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar")
                    |> Expect.equal (Ok True)
        , test "returns False if the task has no tags" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has tags but NOT the given one" <|
            \() ->
                "- [ ] foo #baz #barrrrr #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has a tags that starts with the given one" <|
            \() ->
                "- [ ] foo #bart"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the tag but it is followed by a slash" <|
            \() ->
                "- [ ] foo #bar/"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the tag but it is followed by a subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar")
                    |> Expect.equal (Ok False)
        ]


hasThisTagWithSubtag : Test
hasThisTagWithSubtag =
    describe "hasThisTag - with subtag"
        [ test "returns True if the task has the given subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/baz")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #bar/Baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "BAR/BAZ")
                    |> Expect.equal (Ok True)
        , test "returns False if the task has the subtag but it is followed by a slash" <|
            \() ->
                "- [ ] foo #bar/baz/"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/baz")
                    |> Expect.equal (Ok False)
        , test "returns False if the task has the subtag but it is followed by another subtag" <|
            \() ->
                "- [ ] foo #bar/baz/qux"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/baz")
                    |> Expect.equal (Ok False)
        , test "only matches actual subtags" <|
            \() ->
                "- [ ] foo #bart"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/")
                    |> Expect.equal (Ok False)
        ]


hasThisTagWithSubtagWildcard : Test
hasThisTagWithSubtagWildcard =
    describe "hasThisTag - with subtag wildcard"
        [ test "returns True if the task has a / on the end" <|
            \() ->
                "- [ ] foo #bar/"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/")
                    |> Expect.equal (Ok True)
        , test "is case insensative" <|
            \() ->
                "- [ ] foo #bAr/"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has a subtag" <|
            \() ->
                "- [ ] foo #bar/baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has nested subtags" <|
            \() ->
                "- [ ] foo #bar/baz/qux"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/")
                    |> Expect.equal (Ok True)
        , test "returns True if the task has no slash on the end" <|
            \() ->
                "- [ ] foo #bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.hasThisTag "bar/")
                    |> Expect.equal (Ok True)
        ]


hasTopLevelTags : Test
hasTopLevelTags =
    describe "hasTopLevelTags"
        [ test "returns True if the task has tags" <|
            \() ->
                "- [ ] foo #baz #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.hasTopLevelTags
                    |> Expect.equal (Ok True)
        , test "returns False if the task only has any descendant tasks with tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar #baz #bar #foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.hasTopLevelTags
                    |> Expect.equal (Ok False)
        , test "returns True if the task has only front matter tags" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "tagA" ]) 0)
                    |> Result.map TaskItem.hasTopLevelTags
                    |> Expect.equal (Ok True)
        , test "returns False if the task and all descendant tasks and the front matter have no tags" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList []) 0)
                    |> Result.map TaskItem.hasTopLevelTags
                    |> Expect.equal (Ok False)
        ]


id : Test
id =
    describe "id"
        [ test "returns FNVC1a(filePath):row" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "File A" Nothing TagList.empty 0)
                    |> Result.map TaskItem.id
                    |> Expect.equal (Ok "1414514984:1")
        ]


isCompleted : Test
isCompleted =
    describe "isCompleted"
        [ test "returns False if the checkbox is NOT checked" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok False)
        , test "returns True if the checkbox is checked with an x" <|
            \() ->
                "- [x] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok True)
        , test "returns True if the checkbox is checked with an X" <|
            \() ->
                "- [X] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.isCompleted
                    |> Expect.equal (Ok True)
        ]


isDated : Test
isDated =
    describe "isDated"
        [ test "returns False if there was no date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok False)
        , test "returns False if the date is invalid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "not a date") TagList.empty 0)
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok False)
        , test "returns True if the date is valid" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" (Just "2020-01-07") TagList.empty 0)
                    |> Result.map TaskItem.isDated
                    |> Expect.equal (Ok True)
        ]


isFromFile : Test
isFromFile =
    describe "isFromFile"
        [ test "returns False if the filenames do NOT match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "File A" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.isFromFile "File B")
                    |> Expect.equal (Ok False)
        , test "returns True if the filenames do match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "File A" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.isFromFile "File A")
                    |> Expect.equal (Ok True)
        , test "returns True if the filenames are both blank" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.isFromFile "")
                    |> Expect.equal (Ok True)
        , test "returns False if the filenames do NOT match case" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "File A" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.isFromFile "File a")
                    |> Expect.equal (Ok False)
        , test "returns False if the filenames are a partial match" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "File A" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.isFromFile "File")
                    |> Expect.equal (Ok False)
        , test "matches id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "File A" Nothing TagList.empty 0)
                    |> Result.map (TaskItem.isFromFile "File")
                    |> Expect.equal (Ok False)
        ]


lineNumber : Test
lineNumber =
    describe "lineNumber"
        [ test "is the actual line number if there is no bodyOffset" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing TagList.empty 0)
                    |> Result.map TaskItem.lineNumber
                    |> Expect.equal (Ok 1)
        , test "adds in the bodyOffset" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing TagList.empty 3)
                    |> Result.map TaskItem.lineNumber
                    |> Expect.equal (Ok 4)
        ]


notes : Test
notes =
    describe "notes"
        [ test "returns an empty string if there are no notes" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "")
        , test "returns non-task indented text as notes" <|
            \() ->
                -- ideally this shouldn't need to have a \n on the end!
                "- [ ] foo\n some notes\n"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "some notes")
        , test "returns multiple notes when intersperced with tasks and blank lines" <|
            \() ->
                -- ideally this shouldn't need to have a \n on the end!
                "- [ ] foo\n some notes\n  - [ ] a subtask\n\n  more notes\n  - [ ]invalid subtask\n"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.notes
                    |> Expect.equal (Ok "some notes\nmore notes\n- [ ]invalid subtask")
        ]


originalText : Test
originalText =
    describe "originalText"
        [ test "retains the original line text" <|
            \() ->
                "- [X]  the @due(2019-12-30) task @completed(2020-01-01) title "
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.originalText
                    |> Expect.equal (Ok "- [X]  the @due(2019-12-30) task @completed(2020-01-01) title ")
        , test "retains leading whitepace for the original line text for descendantTasks" <|
            \() ->
                "- [X] task\n   \t - [ ] sub-task"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.descendantTasks
                    |> Result.map (List.map TaskItem.originalText)
                    |> Expect.equal (Ok [ "   \t - [ ] sub-task" ])
        ]


parsing : Test
parsing =
    describe "parsing"
        [ test "only looks at the first line of a multline string" <|
            \() ->
                "- [X] foo\n- [ ] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "consumes <eol> character" <|
            \() ->
                "- [X] foo\n- [ ] bar"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItemHelpers.basicParser
                            |= TaskItemHelpers.basicParser
                        )
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "fails to parse if not given a task" <|
            \() ->
                "- [X] foo"
                    |> Parser.run
                        (Parser.succeed (\first second -> [ first, second ])
                            |= TaskItemHelpers.basicParser
                            |= TaskItemHelpers.basicParser
                        )
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task which ends straight after the ']'" <|
            \() ->
                "- [X]"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task with no title" <|
            \() ->
                "- [X] "
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "parses a task with '*' as the list marker" <|
            \() ->
                "* [X] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (always "success")
                    |> Expect.equal (Ok "success")
        , test "parses a task with '+' as the list marker" <|
            \() ->
                "+ [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (always "success")
                    |> Expect.equal (Ok "success")
        , test "fails to parse a task with '$' as the list marker" <|
            \() ->
                "$ [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is no gap between the title and the ']'" <|
            \() ->
                "- [X]foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        , test "fails to parse a task when there is whitespace before the list marker" <|
            \() ->
                " - [X] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.mapError (always "failed")
                    |> Expect.equal (Err "failed")
        ]


removeTags : Test
removeTags =
    describe "removeTags"
        [ test "removes from a TaskItem with no subtasks" <|
            \() ->
                "- [ ] foo #foo #foo/ #bar #baz #baza #qux"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.removeTags [ "foo", "baz" ])
                    |> Result.map TaskItem.tags
                    |> Result.map TagList.toList
                    |> Result.map List.sort
                    |> Expect.equal (Ok [ "bar", "baza", "foo/", "qux" ])
        , test "removes from a TaskItem and it's subtasks" <|
            \() ->
                "- [ ] foo #foo #foo/\n  - [ ] bar #bar #baz #baza #qux"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.removeTags [ "foo", "baz" ])
                    |> Result.map TaskItem.tags
                    |> Result.map TagList.toList
                    |> Result.map List.sort
                    |> Expect.equal (Ok [ "bar", "baza", "foo/", "qux" ])
        ]


tags : Test
tags =
    describe "tags"
        [ test "returns an empty array for a task with no tags or substasks" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok TagList.empty)
        , test "returns all tags from front matter, the top level, and sub tasks" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2\n  - [ ] bar #tag3"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "tagA", "tagB" ]) 0)
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok (TagList.fromList [ "tag1", "tag2", "tag3", "tagA", "tagB" ]))
        , test "returns unique list of tags" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2\n  - [ ] bar #tag2"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "tag1" ]) 0)
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok (TagList.fromList [ "tag1", "tag2" ]))
        , test "returns the tags in alphabetical order" <|
            \() ->
                "- [ ] foo #tag2 bar #tag1\n  - [ ] bar #tag1"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList []) 0)
                    |> Result.map TaskItem.tags
                    |> Expect.equal (Ok (TagList.fromList [ "tag1", "tag2" ]))
        , test "tags are not included in the title" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "tag3" ]) 0)
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo bar")
        ]


tasksToToggle : Test
tasksToToggle =
    describe "tasksToToggle"
        [ test "returns an empty array if there are no tasks matching the id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 3) <| { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok [])
        , test "returns the TaskItem if it matches the id" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 1) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "returns the sub-TaskItem if it matches the id and @autocomplete is not set" <|
            \() ->
                "- [ ] foo\n  - [ ] bar"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the TaskItem and the sub-TaskItem if the subtask matches the id and @autocomplete is true and all other descendant tasks are complete" <|
            \() ->
                "- [ ] foo @autocomplete(true)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo", "bar" ])
        , test "returns the TaskItem and the sub-TaskItem if the subtask matches the id and @autocomplete is false and all other descendant tasks are complete" <|
            \() ->
                "- [ ] foo @autocomplete(false)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the sub-TaskItem if the subtask matches the id and @autocomplete is set but there are other incomplete descendant tasks" <|
            \() ->
                "- [ ] foo @autocomplete(true)\n  - [ ] bar\n  - [ ] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the sub-TaskItem if the subtask matches the id, @autocomplete is set and the top level task is already completed" <|
            \() ->
                "- [x] foo @autocomplete(true)\n  - [ ] bar\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 2) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "bar" ])
        , test "returns the TaskItem if it is complete, matches and all descendant tasks are already complete" <|
            \() ->
                "- [x] foo @autocomplete(true)\n  - [x] bar\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 1) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "returns the TaskItem if it is incomplete, matches and all descendant tasks are already complete" <|
            \() ->
                "- [ ] foo @autocomplete(true)\n  - [x] bar\n  - [x] baz"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.tasksToToggle (TaskHelpers.taskId "" 1) <| { time = Time.millisToPosix 0 })
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        ]


title : Test
title =
    describe "title"
        [ test "gets the title from a TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "foo")
        , test "handles lots of whitespace between the title and the ']'" <|
            \() ->
                "- [X]      the task"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "the task")
        , test "looses trailing whitespace" <|
            \() ->
                "- [X] the task   "
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.title
                    |> Expect.equal (Ok "the task")
        ]


titleWithTags : Test
titleWithTags =
    describe "titleWithTags"
        [ test "returns the title when there are no #tags or special date/tag fields" <|
            \() ->
                "- [ ]   foo bar    baz   "
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.titleWithTags
                    |> Expect.equal (Ok "foo bar baz")
        , test "returns the title including any #tags when there are no special date/tag fields" <|
            \() ->
                "- [ ]   foo #bar    baz   "
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.titleWithTags
                    |> Expect.equal (Ok "foo #bar baz")
        ]


topLevelTags : Test
topLevelTags =
    describe "topLevelTags"
        [ test "returns an empty array for a task with no tags or substasks" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.topLevelTags
                    |> Expect.equal (Ok TagList.empty)
        , test "returns all tags from front matter and the top level, but NOT sub tasks" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2\n  - [ ] bar #tag3"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "tagA", "tagB" ]) 0)
                    |> Result.map TaskItem.topLevelTags
                    |> Expect.equal (Ok (TagList.fromList [ "tag1", "tag2", "tagA", "tagB" ]))
        , test "returns unique list of tags" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList [ "tag1" ]) 0)
                    |> Result.map TaskItem.topLevelTags
                    |> Expect.equal (Ok (TagList.fromList [ "tag1", "tag2" ]))
        , test "returns the tags in alphabetical order" <|
            \() ->
                "- [ ] foo #tag2 bar #tag1"
                    |> Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion "" Nothing (TagList.fromList []) 0)
                    |> Result.map TaskItem.topLevelTags
                    |> Expect.equal (Ok (TagList.fromList [ "tag1", "tag2" ]))
        ]


topLevelTaskHasThisTag : Test
topLevelTaskHasThisTag =
    describe "topLevelTaskHasThisTag"
        [ test "returns False for (theTag) '- [ ] foo" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.topLevelTaskHasThisTag "theTag")
                    |> Expect.equal (Ok False)
        , test "returns True for (theTag) '- [ ] foo #theTag'" <|
            \() ->
                "- [ ] foo #theTag"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.topLevelTaskHasThisTag "theTag")
                    |> Expect.equal (Ok True)
        , test "returns False for (theTag) '- [ ] foo #otherTag \n  - [ ] bar #otherTag'" <|
            \() ->
                "- [ ] foo #otherTag\n  - [ ] bar #otherTag"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.topLevelTaskHasThisTag "theTag")
                    |> Expect.equal (Ok False)
        , test "returns False for (theTag) '- [ ] foo\n  - [ ] bar #thisTag'" <|
            \() ->
                "- [ ] foo\n  - [ ] bar #theTag"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.topLevelTaskHasThisTag "theTag")
                    |> Expect.equal (Ok False)
        ]


toToggledString : Test
toToggledString =
    describe "toToggledString"
        [ test "given an INCOMPLETE item it outputs a string for a completed task with no completed tag in NoCompletion format" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2 ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString DataviewTaskCompletion.NoCompletion GlobalSettings.NoCompletion { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ^12345")
        , test "given an INCOMPLETE item it outputs a string for a completed task in ObsidianCardBoard format" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2 ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString DataviewTaskCompletion.NoCompletion GlobalSettings.ObsidianCardBoard { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 @completed(1970-01-01T00:00:00) ^12345")
        , test "given an INCOMPLETE item it outputs a string for a completed task in ObsidianTasks format" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2 ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString DataviewTaskCompletion.NoCompletion GlobalSettings.ObsidianTasks { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ✅ 1970-01-01 ^12345")
        , test "given an INCOMPLETE item it outputs a string for a completed task in ObsidianDataview custom format" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2 ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString (DataviewTaskCompletion.Text "done") GlobalSettings.ObsidianDataview { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 [done:: 1970-01-01] ^12345")
        , test "given an INCOMPLETE item it outputs a string for a completed task in ObsidianDataview emoji format" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2 ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString DataviewTaskCompletion.Emoji GlobalSettings.ObsidianDataview { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ✅ 1970-01-01 ^12345")
        , test "given an INCOMPLETE item it outputs a string for a completed task in ObsidianDataview no completion" <|
            \() ->
                "- [ ] foo #tag1 bar #tag2 ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString DataviewTaskCompletion.NoCompletion GlobalSettings.ObsidianDataview { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ^12345")
        , test "given an item with an 'x' in the checkbox outputs a string for an incomplete task removing all formats of completed marks" <|
            \() ->
                "- [x] foo #tag1 bar #tag2 @completed(2020-03-22T00:00:00) ✅ 1970-01-01 [done:: 2021-01-01] ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString (DataviewTaskCompletion.Text "done") GlobalSettings.ObsidianCardBoard { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
        , test "doesn't remove dataview format if it has been set to NoCompletion" <|
            \() ->
                "- [x] foo #tag1 bar #tag2 @completed(2020-03-22T00:00:00) ✅ 1970-01-01 [done:: 2021-01-01] ^12345"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString DataviewTaskCompletion.NoCompletion GlobalSettings.ObsidianCardBoard { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 [done:: 2021-01-01] ^12345")
        , test "given an item with an 'X' in the checkbox outputs a string for an incomplete task removing all formats of completed marks" <|
            \() ->
                "- [X] [custom:: 2021-01-01] foo #tag1 ✅ 1970-01-01 @completed(2020-03-22T00:00:00) bar #tag2"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (TaskItem.toToggledString (DataviewTaskCompletion.Text "custom") GlobalSettings.ObsidianCardBoard { time = Time.millisToPosix 0 })
                    |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2")
        , test "preserves leading whitespace for descendant tasks" <|
            \() ->
                "- [X] the task\n   \t- [ ] a subtask"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map TaskItem.descendantTasks
                    |> Result.withDefault []
                    |> List.map (TaskItem.toToggledString DataviewTaskCompletion.NoCompletion GlobalSettings.ObsidianCardBoard { time = Time.millisToPosix 0 })
                    |> Expect.equal [ "   \t- [x] a subtask @completed(1970-01-01T00:00:00)" ]
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
