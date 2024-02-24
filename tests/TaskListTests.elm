module TaskListTests exposing (suite)

import DataviewTaskCompletion
import Expect
import Helpers.DecodeHelpers as DecodeTestHelpers
import Helpers.TaskHelpers as TaskHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import MarkdownFile exposing (MarkdownFile)
import Parser
import TagList
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ add
        , combine
        , decoder
        , encoder
        , filter
        , fromList
        , fromMarkdown
        , map
        , markdownDiffs
        , replaceTaskItems
        , taskContainingId
        , taskFromId
        , toList
        ]


add : Test
add =
    describe "add"
        [ test "adds a TaskItem to an empty TaskList" <|
            \() ->
                TaskList.empty
                    |> TaskList.add (taskItem "- [ ] foo")
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo" ]
        , test "adds a TaskItem to the start of a non empty TaskList" <|
            \() ->
                TaskList.empty
                    |> TaskList.add (taskItem "- [ ] foo")
                    |> TaskList.add (taskItem "- [ ] bar")
                    |> TaskList.taskTitles
                    |> Expect.equal [ "bar", "foo" ]
        ]


combine : Test
combine =
    describe "combine"
        [ test "append joins two TaskLists" <|
            \() ->
                TaskListHelpers.taskListFromFileG
                    |> TaskList.append TaskListHelpers.taskListFromFileA
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2", "g1", "g2" ]
        , test "concat concatinates a list of TaskLists into a single TaskList" <|
            \() ->
                [ TaskListHelpers.taskListFromFileG, TaskListHelpers.taskListFromFileA ]
                    |> TaskList.concat
                    |> TaskList.taskTitles
                    |> Expect.equal [ "g1", "g2", "a1", "a2" ]
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes an empty TaskList" <|
            \() ->
                "[]"
                    |> DecodeTestHelpers.runDecoder TaskList.decoder
                    |> .decoded
                    |> Expect.equal (Ok TaskList.empty)
        , test "decodes a TaskList containing tasks" <|
            \() ->
                let
                    taskList : TaskList
                    taskList =
                        TaskList.empty
                            |> TaskList.add (taskItem "- [ ] foo")
                            |> TaskList.add (taskItem "- [ ] bar")
                in
                """[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] bar","originalLine":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]},{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] foo","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]}]"""
                    |> DecodeTestHelpers.runDecoder TaskList.decoder
                    |> .decoded
                    |> Expect.equal (Ok taskList)
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes an empty TaskList" <|
            \() ->
                TaskList.empty
                    |> TsEncode.runExample TaskList.encoder
                    |> .output
                    |> Expect.equal """[]"""
        , test "encodes an TaskList containing tasks" <|
            \() ->
                TaskList.empty
                    |> TaskList.add (taskItem "- [ ] foo")
                    |> TaskList.add (taskItem "- [ ] bar")
                    |> TsEncode.runExample TaskList.encoder
                    |> .output
                    |> Expect.equal """[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] bar","originalLine":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]},{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] foo","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]}]"""
        ]


filter : Test
filter =
    describe "filter"
        [ test "returns an empty TaskList if given an one" <|
            \() ->
                ""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.filter (always True)
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "filters a TaskList based on a TaskItem property" <|
            \() ->
                """- [ ] foo
- [x] bar #tag1
- [X] baz #tag2
- [X] boo
"""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.filter TaskItem.hasTags
                    |> TaskList.taskTitles
                    |> Expect.equal [ "bar", "baz" ]
        ]


fromList : Test
fromList =
    describe "fromList"
        [ test "returns an empty TaskList if given an empty list" <|
            \() ->
                []
                    |> TaskList.fromList
                    |> Expect.equal TaskList.empty
        , test "returns a TaskList containing the items in the list" <|
            \() ->
                [ taskItem "- [ ] foo", taskItem "- [ ] bar" ]
                    |> TaskList.fromList
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        ]


fromMarkdown : Test
fromMarkdown =
    describe "todo fromMarkdown"
        [ test "parses an empty file" <|
            \() ->
                ""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> Expect.equal TaskList.empty
        , test "parses a single incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo" ]
        , test "parses a contiguous block of TaskList items" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses non contiguous TaskList items" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses TaskList items with non-tasks interspersed" <|
            \() ->
                """- [ ] foo

not a task

- [x] bar

- [X] baz

"""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "ignores indented tasks" <|
            \() ->
                """- [ ] foo

not a task

- [x] bar

- [X] baz
  - [ ] a subtask

"""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the first line of the file is blank" <|
            \() ->
                """
- [ ] foo
- [x] bar
- [X] baz
"""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks ignoring any that don't have a title" <|
            \() ->
                """
- [ ] foo
- [ ] 
- [x] bar
- [X] baz
"""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the last line is a task and has NO line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar"
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses tasks when the last line is a non-task and has a line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar\n\n## Log\n"
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses ids consiting of the filePath and line number" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> basicMarkdown "file_a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.taskIds
                    |> Expect.equal [ "4275677999:1", "4275677999:4", "4275677999:6" ]
        , test "adds frontmatter tags to all the tasks" <|
            \() ->
                """- [ ] foo
- [x] bar
"""
                    |> basicMarkdown "file_a"
                    |> (\md -> { md | frontMatterTags = TagList.fromList [ "fm_tag1", "fm_tag2" ] })
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.toList
                    |> List.map TaskItem.tags
                    |> Expect.equal
                        [ TagList.fromList [ "fm_tag1", "fm_tag2" ]
                        , TagList.fromList [ "fm_tag1", "fm_tag2" ]
                        ]
        ]


markdownDiffs : Test
markdownDiffs =
    describe "markdownDiffs"
        [ test "returns an empty diff if both the original an updated files are empty" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" ""
                in
                ""
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal { toAdd = [], toDelete = [] }
        , test "returns an empty diff if both the original an updated files are the same" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "- [ ] foo"
                in
                "- [ ] foo"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal { toAdd = [], toDelete = [] }
        , test "returns a an empty diff if non-task line is edited" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "# title\n- [ ] foo"
                in
                "# LONGER title\n- [ ] foo"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal { toAdd = [], toDelete = [] }
        , test "returns a task to add if one has been added" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "# title\n- [ ] foo\n- [ ] bar"
                in
                "# title\n- [ ] foo"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal
                        { toAdd = [ taskItemPlus "a" 2 "- [ ] bar" ]
                        , toDelete = []
                        }
        , test "returns a task to remove if one has been removed" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "# title\n- [ ] foo"
                in
                "# title\n- [ ] foo\n- [ ] bar"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal
                        { toAdd = []
                        , toDelete = [ taskItemPlus "a" 2 "- [ ] bar" ]
                        }
        , test "returns tasks to remove and add if one has been edited" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "# title\n- [ ] xxx"
                in
                "# title\n- [ ] foo"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal
                        { toAdd = [ taskItemPlus "a" 1 "- [ ] xxx" ]
                        , toDelete = [ taskItemPlus "a" 1 "- [ ] foo" ]
                        }
        , test "returns deletes and adds if it has had a subtask added" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "# title\n- [ ] foo\n- [ ] bar\n - [ ] baz"
                in
                "# title\n- [ ] foo\n- [ ] bar"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal
                        { toAdd = [ taskItemPlus "a" 2 "- [ ] bar\n - [ ] baz" ]
                        , toDelete = [ taskItemPlus "a" 2 "- [ ] bar" ]
                        }
        , test "returns deletes and adds if it has had a subtask edited" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "# title\n- [ ] foo\n- [ ] bar\n - [ ] bazzer"
                in
                "# title\n- [ ] foo\n- [ ] bar\n - [ ] baz"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal
                        { toAdd = [ taskItemPlus "a" 2 "- [ ] bar\n - [ ] bazzer" ]
                        , toDelete = [ taskItemPlus "a" 2 "- [ ] bar\n - [ ] baz" ]
                        }
        , test "returns deletes and adds if it has had notes deleted" <|
            \() ->
                let
                    updatedMarkdown : MarkdownFile
                    updatedMarkdown =
                        basicMarkdown "a" "# title\n- [ ] foo"
                in
                "# title\n- [ ] foo\n  some notes\n"
                    |> basicMarkdown "a"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.markdownDiffs DataviewTaskCompletion.NoCompletion updatedMarkdown
                    |> Expect.equal
                        { toAdd = [ taskItemPlus "a" 1 "- [ ] foo" ]
                        , toDelete = [ taskItemPlus "a" 1 "- [ ] foo\n  some notes\n" ]
                        }
        ]


map : Test
map =
    describe "map"
        [ test "returns an empty TaskList if given an one" <|
            \() ->
                ""
                    |> basicMarkdown ""
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> TaskList.map identity
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "maps the contents of a TaskList throgh a function" <|
            \() ->
                """- [ ] foo
- [x] bar #tag1
"""
                    |> basicMarkdown "old/path"
                    |> TaskList.fromMarkdown DataviewTaskCompletion.NoCompletion
                    |> (TaskList.map <| TaskItem.updateFilePath "old/path" "new/path")
                    |> TaskList.topLevelTasks
                    |> List.map TaskItem.filePath
                    |> Expect.equal [ "new/path", "new/path" ]
        ]


replaceTaskItems : Test
replaceTaskItems =
    describe "replaceTaskItems"
        [ test "does nothing if there are no replacement details" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] foo
- [x] bar
""" )
                    |> TaskList.replaceTaskItems []
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        , test "replaces TaskItems if the id matches" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] foo
- [x] bar
""" )
                    |> TaskList.replaceTaskItems [ ( "3826002220:3", taskItem "- [ ] baz" ) ]
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "foo", "baz" ])
        ]


taskContainingId : Test
taskContainingId =
    describe "taskContainingId"
        [ test "returns nothing if there are no tasks in the list" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, "" )
                    |> TaskList.taskContainingId ""
                    |> Expect.equal Nothing
        , test "returns nothing if there are no tasks in the list with the given id" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskContainingId (TaskHelpers.taskId "a" 4)
                    |> Expect.equal Nothing
        , test "returns the task if there is one in the list with the given id" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskContainingId (TaskHelpers.taskId "a" 3)
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "g2")
        , test "returns the task if it contains a  subtask with the given id" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
  - [x] subtask complete
""" )
                    |> TaskList.taskContainingId (TaskHelpers.taskId "a" 3)
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "g1")
        ]


taskFromId : Test
taskFromId =
    describe "taskFromId"
        [ test "returns nothing if there are no tasks in the list" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, "" )
                    |> TaskList.taskFromId ""
                    |> Expect.equal Nothing
        , test "returns nothing if there are no tasks in the list with the given id" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskFromId (TaskHelpers.taskId "a" 4)
                    |> Expect.equal Nothing
        , test "returns the task if there is one in the list with the given id" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskFromId (TaskHelpers.taskId "a" 3)
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "g2")
        , test "returns a subtask if there is one in the list with the given id" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
  - [x] subtask complete
""" )
                    |> TaskList.taskFromId (TaskHelpers.taskId "a" 3)
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "subtask complete")
        ]


toList : Test
toList =
    describe "tasks"
        [ test "returns an empty list if there are no tasks" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, "" )
                    |> TaskList.toList
                    |> Expect.equal []
        , test "returns a list of all tasks and subtasks" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
  - [x] subtask complete
""" )
                    |> TaskList.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "g1", "subtask complete" ]
        ]



-- HELPERS


basicMarkdown : String -> String -> MarkdownFile
basicMarkdown path body =
    { filePath = path
    , fileDate = Nothing
    , frontMatterTags = TagList.empty
    , bodyOffset = 0
    , body = body
    }


taskItemPlus : String -> Int -> String -> TaskItem
taskItemPlus file offset markdown =
    Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion file Nothing TagList.empty offset) markdown
        |> Result.withDefault TaskItem.dummy


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
