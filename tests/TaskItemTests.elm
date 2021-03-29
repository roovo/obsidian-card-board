module TaskItemTests exposing (suite)

import Date
import Expect exposing (Expectation)
import Parser
import TaskItem exposing (Completion(..), TaskItem)
import TaskList exposing (TaskList)
import Test exposing (..)


suite : Test
suite =
    concat
        [ parsing
        , filtering
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
                """- [X] foo
                - [ ] bar
                """
                    |> Parser.run (TaskItem.parser "" Nothing)
                    |> Result.map (\t -> TaskItem.title t)
                    |> Expect.equal (Ok "foo")
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


filtering : Test
filtering =
    describe "filtering"
        [ test "undated" <|
            \() ->
                parsedFiles
                    |> TaskList.taskItems
                    |> TaskItem.undated
                    |> List.map TaskItem.title
                    |> Expect.equal [ "undated incomplete", "invalid date incomplete", "chosen file incomplete" ]
        , test "forToday" <|
            \() ->
                parsedFiles
                    |> TaskList.taskItems
                    |> TaskItem.forToday todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "yesterday incomplete", "today incomplete" ]
        , test "forTommorrow" <|
            \() ->
                parsedFiles
                    |> TaskList.taskItems
                    |> TaskItem.forTomorrow todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "tomorrow incomplete" ]
        , test "forFuture" <|
            \() ->
                parsedFiles
                    |> TaskList.taskItems
                    |> TaskItem.forFuture todayAsDate
                    |> List.map TaskItem.title
                    |> Expect.equal [ "future incomplete" ]
        , test "completed" <|
            \() ->
                parsedFiles
                    |> TaskList.taskItems
                    |> TaskItem.completed
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
        , test "fromFile" <|
            \() ->
                parsedFiles
                    |> TaskList.taskItems
                    |> TaskItem.fromFile "/path/to/file"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "chosen file incomplete", "chosen file complete" ]
        , test "notFromFile" <|
            \() ->
                parsedFiles
                    |> TaskList.taskItems
                    |> TaskItem.notFromFile "/path/to/file"
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "undated incomplete"
                        , "undated complete"
                        , "yesterday incomplete"
                        , "yesterday complete"
                        , "today incomplete"
                        , "today complete"
                        , "tomorrow incomplete"
                        , "tomorrow complete"
                        , "future incomplete"
                        , "future complete"
                        , "invalid date incomplete"
                        , "invalid date complete"
                        ]
        ]


parsedFiles : TaskList
parsedFiles =
    taskFiles
        |> List.map (\( p, d, ts ) -> Parser.run (TaskList.parser p d) ts)
        |> List.map (Result.withDefault TaskList.empty)
        |> TaskList.concat


todayAsDate : Date.Date
todayAsDate =
    today
        |> Date.fromIsoString
        |> Result.map Date.toRataDie
        |> Result.withDefault 0
        |> Date.fromRataDie


taskFiles : List ( String, Maybe String, String )
taskFiles =
    [ ( "", Nothing, """
- [ ] undated incomplete
- [x] undated complete
""" )
    , ( "", Just yesterday, """
- [ ] yesterday incomplete
- [x] yesterday complete
""" )
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
    , ( "/path/to/file", Nothing, """
- [ ] chosen file incomplete
- [x] chosen file complete
""" )
    ]


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
