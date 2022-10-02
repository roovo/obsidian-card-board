module TaskListTests exposing (suite)

import Expect
import Helpers.TaskHelpers as TaskHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import Parser
import TagList
import TaskItem
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ combine
        , filter
        , map
        , parsing
        , replaceForFile
        , removeForFile
        , taskContainingId
        , taskFromId
        , tasks
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


filter : Test
filter =
    describe "filter"
        [ test "returns an empty TaskList if given an one" <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.map (TaskList.filter (always True))
                    |> Result.map TaskList.taskTitles
                    |> Expect.equal (Ok [])
        , test "filters a TaskList based on a TaskItem property" <|
            \() ->
                """- [ ] foo
- [x] bar #tag1
- [X] baz #tag2
- [X] boo
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.map (TaskList.filter TaskItem.hasTags)
                    |> Result.map TaskList.taskTitles
                    |> Expect.equal (Ok [ "bar", "baz" ])
        ]


map : Test
map =
    describe "map"
        [ test "returns an empty TaskList if given an one" <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.map (TaskList.map identity)
                    |> Result.map TaskList.taskTitles
                    |> Expect.equal (Ok [])
        , test "maps the contents of a TaskList throgh a function" <|
            \() ->
                """- [ ] foo
- [x] bar #tag1
"""
                    |> Parser.run (TaskList.parser "old/path" Nothing TagList.empty 0)
                    |> Result.map (TaskList.map <| TaskItem.updateFilePath "old/path" "new/path")
                    |> Result.map TaskList.topLevelTasks
                    |> Result.map (List.map TaskItem.filePath)
                    |> Expect.equal (Ok [ "new/path", "new/path" ])
        ]


parsing : Test
parsing =
    describe "todo parsing"
        [ test "parses an empty file" <|
            \() ->
                ""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "parses a single incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo" ]
        , test "parses a contiguous block of TaskList items" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses non contiguous TaskList items" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses TaskList items with non-tasks interspersed" <|
            \() ->
                """- [ ] foo

not a task

- [x] bar

- [X] baz

"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
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
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the first line of the file is blank" <|
            \() ->
                """
- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
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
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the last line is a task and has NO line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar"
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses tasks when the last line is a non-task and has a line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar\n\n## Log\n"
                    |> Parser.run TaskListHelpers.basicParser
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses ids consiting of the filePath and line number" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Parser.run (TaskList.parser "file_a" Nothing TagList.empty 0)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskIds
                    |> Expect.equal [ "4275677999:1", "4275677999:4", "4275677999:6" ]
        , test "adds frontmatter tags to the tasks" <|
            \() ->
                """- [ ] foo
- [x] bar
"""
                    |> Parser.run (TaskList.parser "file_a" Nothing (TagList.fromList [ "fm_tag1", "fm_tag2" ]) 0)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.tasks
                    |> List.map TaskItem.allTags
                    |> Expect.equal
                        [ TagList.fromList [ "fm_tag1", "fm_tag2" ]
                        , TagList.fromList [ "fm_tag1", "fm_tag2" ]
                        ]
        ]


replaceForFile : Test
replaceForFile =
    describe "replacing tasks from a chosen file"
        [ test "adds the tasks if the TaskList is empty" <|
            \() ->
                TaskList.empty
                    |> TaskList.replaceForFile "ignored"
                        (TaskListHelpers.taskListFromFile "file a")
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "c1", "c2" ])
        , test "adds the tasks if the list doesn't contain tasks from the file" <|
            \() ->
                TaskListHelpers.taskListFromFileG
                    |> TaskList.replaceForFile "ignored"
                        (TaskListHelpers.taskListFromFile "file a")
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "g1", "g2", "c1", "c2" ])
        , test "replaces tasks from the file" <|
            \() ->
                TaskListHelpers.taskListFromFileG
                    |> TaskList.append (TaskListHelpers.taskListFromFile "file a")
                    |> TaskList.replaceForFile "file a"
                        (TaskListHelpers.taskListFromNewFile "could be another file")
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "n1", "n2", "g1", "g2" ])
        ]


removeForFile : Test
removeForFile =
    describe "removing tasks from a chosen file"
        [ test "has no effect if the TaskList is empty" <|
            \() ->
                TaskList.empty
                    |> TaskList.removeForFile "a file"
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [])
        , test "remove tasks from the file" <|
            \() ->
                TaskListHelpers.taskListFromFileG
                    |> TaskList.append (TaskListHelpers.taskListFromFile "file a")
                    |> TaskList.removeForFile "file a"
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "g1", "g2" ])
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


tasks : Test
tasks =
    describe "tasks"
        [ test "returns an empty list if there are no tasks" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, "" )
                    |> TaskList.tasks
                    |> Expect.equal []
        , test "returns a list of all tasks and subtasks" <|
            \() ->
                TaskListHelpers.parsedTasks ( "a", Nothing, """
- [ ] g1
  - [x] subtask complete
""" )
                    |> TaskList.tasks
                    |> List.map TaskItem.title
                    |> Expect.equal [ "g1", "subtask complete" ]
        ]
