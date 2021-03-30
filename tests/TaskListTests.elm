module TaskListTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Parser
import TaskItem exposing (Completion(..), TaskItem)
import TaskList exposing (TaskList)
import Test exposing (..)


suite : Test
suite =
    concat
        [ combine
        , filtering
        , replaceForFile
        , parsing
        , parsingToFix
        ]


parsingToFix : Test
parsingToFix =
    describe "todo parsing it would be good to fix (save appending \n to end of string before parsing"
        [ test "FAILS to parse tasks when the last line is a non-task and has NO line ending" <|
            \() ->
                "- [ ] foo\na"
                    |> Parser.run (TaskList.parser "" Nothing)
                    |> Result.withDefault TaskList.empty
                    |> TaskList.taskTitles
                    |> Expect.equal []
        ]


combine : Test
combine =
    describe "combine"
        [ test "append joins two TaskLists" <|
            \() ->
                parsedTasks undatedTasks
                    |> TaskList.append (parsedTasks yesterdaysTasks)
                    |> TaskList.taskTitles
                    |> Expect.equal [ "yesterday incomplete", "yesterday complete", "undated incomplete", "undated complete" ]
        , test "concat concatinates a list of TaskLists into a single TaskList" <|
            \() ->
                [ parsedTasks undatedTasks, parsedTasks yesterdaysTasks ]
                    |> TaskList.concat
                    |> TaskList.taskTitles
                    |> Expect.equal [ "undated incomplete", "undated complete", "yesterday incomplete", "yesterday complete" ]
        ]


filtering : Test
filtering =
    describe "filtering"
        [ test "undatedItems" <|
            \() ->
                parsedFiles
                    |> TaskList.undatedItems
                    |> List.map TaskItem.title
                    |> Expect.equal [ "undated incomplete", "invalid date incomplete", "chosen file incomplete" ]
        , test "todaysItems" <|
            \() ->
                parsedFiles
                    |> TaskList.todaysItems todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "yesterday incomplete", "today incomplete" ]
        , test "tommorrowsItems" <|
            \() ->
                parsedFiles
                    |> TaskList.tomorrowsItems todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "tomorrow incomplete" ]
        , test "futureItems" <|
            \() ->
                parsedFiles
                    |> TaskList.futureItems todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "future incomplete" ]
        , test "completedItems" <|
            \() ->
                parsedFiles
                    |> TaskList.completedItems
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "undated complete"
                        , "yesterday complete"
                        , "today complete"
                        , "tomorrow complete"
                        , "future complete"
                        , "invalid date complete"
                        , "chosen file complete"
                        ]
        ]



-- replaceForFile filePath updatedList currentList =


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
                    |> Expect.equal (List.sort [ "chosen file incomplete", "chosen file complete" ])
        , test "adds the tasks if the list doesn't contain tasks from the file" <|
            \() ->
                parsedTasks undatedTasks
                    |> TaskList.replaceForFile "ignored"
                        (parsedTasks <| tasksFromFile "file a")
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "undated incomplete", "undated complete", "chosen file incomplete", "chosen file complete" ])
        , test "replaces tasks from the file" <|
            \() ->
                parsedTasks undatedTasks
                    |> TaskList.append (parsedTasks <| tasksFromFile "file a")
                    |> TaskList.replaceForFile "file a"
                        (parsedTasks <| tasksFromNewFile "could be another file")
                    |> TaskList.taskTitles
                    |> List.sort
                    |> Expect.equal (List.sort [ "new file incomplete", "new file complete", "undated incomplete", "undated complete" ])
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
        ]


parsedFiles : TaskList
parsedFiles =
    taskFiles
        |> List.map parsedTasks
        |> TaskList.concat


parsedTasks : ( String, Maybe String, String ) -> TaskList
parsedTasks ( p, d, ts ) =
    Parser.run (TaskList.parser p d) ts
        |> Result.withDefault TaskList.empty


todayAsDate : Date.Date
todayAsDate =
    today
        |> Date.fromIsoString
        |> Result.map Date.toRataDie
        |> Result.withDefault 0
        |> Date.fromRataDie


taskFiles : List ( String, Maybe String, String )
taskFiles =
    [ undatedTasks
    , yesterdaysTasks
    , ( "", Just today, """
- [ ] today incomplete
- [x] today complete
""" )
    , ( "", Just tomorrow, """
- [ ] tomorrow incomplete
- [x] tomorrow complete
""" )
    , ( "", Just future, """
- [ ] future incomplete
- [x] future complete
""" )
    , ( "", Just "invalid date", """
- [ ] invalid date incomplete
- [x] invalid date complete
""" )
    , tasksFromFile "/path/to/file"
    ]


undatedTasks : ( String, Maybe String, String )
undatedTasks =
    ( "", Nothing, """
- [ ] undated incomplete
- [x] undated complete
""" )


yesterdaysTasks : ( String, Maybe String, String )
yesterdaysTasks =
    ( "", Just yesterday, """
- [ ] yesterday incomplete
- [x] yesterday complete
""" )


tasksFromFile : String -> ( String, Maybe String, String )
tasksFromFile path =
    ( path, Nothing, """
- [ ] chosen file incomplete
- [x] chosen file complete
""" )


tasksFromNewFile : String -> ( String, Maybe String, String )
tasksFromNewFile path =
    ( path, Nothing, """
- [ ] new file incomplete
- [x] new file complete
""" )


yesterday : String
yesterday =
    "2020-06-19"


today : String
today =
    "2020-06-20"


tomorrow : String
tomorrow =
    "2020-06-21"


future : String
future =
    "2020-06-22"
