module TaskListTests exposing (suite)

import Date
import Expect
import Parser
import TaskItem
import TaskList exposing (TaskList)
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
                parsedTasks tasksFromFileG
                    |> TaskList.append (parsedTasks tasksFromFileA)
                    |> TaskList.taskTitles
                    |> Expect.equal [ "a1", "a2", "g1", "g2" ]
        , test "concat concatinates a list of TaskLists into a single TaskList" <|
            \() ->
                [ parsedTasks tasksFromFileG, parsedTasks tasksFromFileA ]
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
                    |> Parser.run (TaskList.parser "" Nothing)
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
                    |> Parser.run (TaskList.parser "" Nothing)
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
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.map (TaskList.map identity)
                    |> Result.map TaskList.taskTitles
                    |> Expect.equal (Ok [])
        , test "maps the contents of a TaskList throgh a function" <|
            \() ->
                """- [ ] foo
- [x] bar #tag1
"""
                    |> Parser.run (TaskList.parser "old/path" Nothing)
                    |> Result.map (TaskList.map <| TaskItem.updateFilePath "new/path")
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
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal []
        , test "parses a single incomplete TaskList item" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo" ]
        , test "parses a contiguous block of TaskList items" <|
            \() ->
                """- [ ] foo
- [x] bar
- [X] baz
"""
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses non contiguous TaskList items" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Parser.run (TaskList.parser "" Nothing)
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
                    |> Parser.run (TaskList.parser "" Nothing)
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
                    |> Parser.run (TaskList.parser "" Nothing)
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
                    |> Parser.run (TaskList.parser "" Nothing)
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
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar", "baz" ]
        , test "parses tasks when the last line is a task and has NO line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar"
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses tasks when the last line is a non-task and has a line ending" <|
            \() ->
                "- [ ] foo\n- [x] bar\n\n## Log\n"
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal [ "foo", "bar" ]
        , test "parses ids consiting of the filePath and line number" <|
            \() ->
                """- [ ] foo


- [x] bar

- [X] baz

"""
                    |> Parser.run (TaskList.parser "file_a" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskIds
                    |> Expect.equal [ "file_a:1", "file_a:4", "file_a:6" ]
        ]


replaceForFile : Test
replaceForFile =
    describe "replacing tasks from a chosen file"
        [ test "adds the tasks if the TaskList is empty" <|
            \() ->
                TaskList.empty
                    |> TaskList.replaceForFile "ignored"
                        (parsedTasks <| tasksFromFile "file a")
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "c1", "c2" ])
        , test "adds the tasks if the list doesn't contain tasks from the file" <|
            \() ->
                parsedTasks tasksFromFileG
                    |> TaskList.replaceForFile "ignored"
                        (parsedTasks <| tasksFromFile "file a")
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "g1", "g2", "c1", "c2" ])
        , test "replaces tasks from the file" <|
            \() ->
                parsedTasks tasksFromFileG
                    |> TaskList.append (parsedTasks <| tasksFromFile "file a")
                    |> TaskList.replaceForFile "file a"
                        (parsedTasks <| tasksFromNewFile "could be another file")
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
                parsedTasks tasksFromFileG
                    |> TaskList.append (parsedTasks <| tasksFromFile "file a")
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
                parsedTasks ( "a", Nothing, "" )
                    |> TaskList.taskContainingId ""
                    |> Expect.equal Nothing
        , test "returns nothing if there are no tasks in the list with the given id" <|
            \() ->
                parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskContainingId "a:4"
                    |> Expect.equal Nothing
        , test "returns the task if there is one in the list with the given id" <|
            \() ->
                parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskContainingId "a:3"
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "g2")
        , test "returns the task if it contains a  subtask with the given id" <|
            \() ->
                parsedTasks ( "a", Nothing, """
- [ ] g1
  - [x] subtask complete
""" )
                    |> TaskList.taskContainingId "a:3"
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "g1")
        ]


taskFromId : Test
taskFromId =
    describe "taskFromId"
        [ test "returns nothing if there are no tasks in the list" <|
            \() ->
                parsedTasks ( "a", Nothing, "" )
                    |> TaskList.taskFromId ""
                    |> Expect.equal Nothing
        , test "returns nothing if there are no tasks in the list with the given id" <|
            \() ->
                parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskFromId "a:4"
                    |> Expect.equal Nothing
        , test "returns the task if there is one in the list with the given id" <|
            \() ->
                parsedTasks ( "a", Nothing, """
- [ ] g1
- [x] g2
""" )
                    |> TaskList.taskFromId "a:3"
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "g2")
        , test "returns a subtask if there is one in the list with the given id" <|
            \() ->
                parsedTasks ( "a", Nothing, """
- [ ] g1
  - [x] subtask complete
""" )
                    |> TaskList.taskFromId "a:3"
                    |> Maybe.map TaskItem.title
                    |> Expect.equal (Just "subtask complete")
        ]


tasks : Test
tasks =
    describe "tasks"
        [ test "returns an empty list if there are no tasks" <|
            \() ->
                parsedTasks ( "a", Nothing, "" )
                    |> TaskList.tasks
                    |> Expect.equal []
        , test "returns a list of all tasks and subtasks" <|
            \() ->
                parsedTasks ( "a", Nothing, """
- [ ] g1
  - [x] subtask complete
""" )
                    |> TaskList.tasks
                    |> List.map TaskItem.title
                    |> Expect.equal [ "g1", "subtask complete" ]
        ]



-- HELPERS


parsedTasks : ( String, Maybe String, String ) -> TaskList
parsedTasks ( p, d, ts ) =
    Parser.run (TaskList.parser p d) ts
        |> Result.withDefault TaskList.empty


tasksFromFileG : ( String, Maybe String, String )
tasksFromFileG =
    ( "g", Nothing, """
- [ ] g1
- [x] g2
""" )


tasksFromFileA : ( String, Maybe String, String )
tasksFromFileA =
    ( "a", Nothing, """
- [ ] a1
- [x] a2
""" )


tasksFromFile : String -> ( String, Maybe String, String )
tasksFromFile path =
    ( path, Nothing, """
- [ ] c1
- [x] c2
""" )


tasksFromNewFile : String -> ( String, Maybe String, String )
tasksFromNewFile path =
    ( path, Nothing, """
- [ ] n1
- [x] n2
""" )
