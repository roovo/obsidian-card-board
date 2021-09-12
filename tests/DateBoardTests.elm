module DateBoardTests exposing (suite)

import DateBoard exposing (DateBoard)
import Expect
import Iso8601
import Parser
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Test exposing (..)
import Time


suite : Test
suite =
    concat
        [ columns
        , columnCompleted
        , columnUndated
        ]


columns : Test
columns =
    describe "columns"
        [ test "default columns are just today tomorrow and future" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill defaultConfig
                    |> DateBoard.columns now zone
                    |> List.map Tuple.first
                    |> Expect.equal [ "Today", "Tomorrow", "Future" ]
        , test "todaysItems are sorted by due date (then task title ascending)" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill defaultConfig
                    |> DateBoard.columns now zone
                    |> tasksInColumn "Today"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "another yesterday incomplete", "yesterday incomplete", "today incomplete" ]
        , test "tommorrowsItems are sorted by task title ascending" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill defaultConfig
                    |> DateBoard.columns now zone
                    |> tasksInColumn "Tomorrow"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a task for tomorrow", "tomorrow incomplete" ]
        , test "futureItems are sorted by due date ascending (then task title)" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill defaultConfig
                    |> DateBoard.columns now zone
                    |> tasksInColumn "Future"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "future incomplete", "far future incomplete", "zapping into the future" ]
        ]


columnCompleted : Test
columnCompleted =
    describe "columnCompleted"
        [ test "a Completed column is appended if config sets includeCompleted" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill { defaultConfig | includeCompleted = True }
                    |> DateBoard.columns now zone
                    |> List.map Tuple.first
                    |> Expect.equal [ "Today", "Tomorrow", "Future", "Done" ]
        , test "completedItems are sorted by completion date desc (then task title asc)" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill { defaultConfig | includeCompleted = True }
                    |> DateBoard.columns now zone
                    |> tasksInColumn "Done"
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "future complete"
                        , "today complete"
                        , "tomorrow complete"
                        , "undated complete"
                        , "yesterday complete"
                        , "far future complete"
                        , "invalid date complete"
                        ]
        ]


columnUndated : Test
columnUndated =
    describe "columnUndated"
        [ test "an Undated column is prepended if config sets includeUndated" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill { defaultConfig | includeUndated = True }
                    |> DateBoard.columns now zone
                    |> List.map Tuple.first
                    |> Expect.equal [ "Undated", "Today", "Tomorrow", "Future" ]
        , test "undatedItems are sorted by title ascending" <|
            \() ->
                parsedFiles
                    |> DateBoard.fill { defaultConfig | includeUndated = True }
                    |> DateBoard.columns now zone
                    |> tasksInColumn "Undated"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "an undated incomplete", "invalid date incomplete" ]
        ]



-- HELPERS


defaultConfig : DateBoard.Config
defaultConfig =
    { includeCompleted = False
    , includeUndated = False
    }


tasksInColumn : String -> List ( String, List TaskItem ) -> List TaskItem
tasksInColumn columnName tasksInColumns =
    tasksInColumns
        |> List.filter (\( c, ts ) -> c == columnName)
        |> List.concatMap Tuple.second


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


farFuture : String
farFuture =
    "2020-06-23"


now : Time.Posix
now =
    today
        |> Iso8601.toTime
        |> Result.map Time.posixToMillis
        |> Result.withDefault 0
        |> Time.millisToPosix


zone : Time.Zone
zone =
    Time.utc


parsedFiles : TaskList
parsedFiles =
    taskFiles
        |> List.map parsedTasks
        |> TaskList.concat


parsedTasks : ( String, Maybe String, String ) -> TaskList
parsedTasks ( p, d, ts ) =
    Parser.run (TaskList.parser p d) ts
        |> Result.withDefault TaskList.empty


taskFiles : List ( String, Maybe String, String )
taskFiles =
    [ undatedTasks
    , ( "d", Just farFuture, """
- [ ] zapping into the future
- [ ] far future incomplete
- [x] far future complete
""" )
    , ( "e", Just future, """
- [ ] future incomplete
- [x] future complete @done(2020-06-02)
""" )
    , ( "c", Just tomorrow, """
- [ ] tomorrow incomplete
- [ ] a task for tomorrow
- [x] tomorrow complete @done(2020-06-02)
""" )
    , ( "b", Just today, """
- [ ] today incomplete
- [x] today complete @done(2020-06-02)
""" )
    , yesterdaysTasks
    , ( "f", Just "invalid date", """
- [ ] invalid date incomplete
- [x] invalid date complete
""" )
    ]


undatedTasks : ( String, Maybe String, String )
undatedTasks =
    ( "g", Nothing, """
- [ ] an undated incomplete
- [x] undated complete @done(2020-06-02)
""" )


yesterdaysTasks : ( String, Maybe String, String )
yesterdaysTasks =
    ( "a", Just yesterday, """
- [ ] yesterday incomplete
- [ ] another yesterday incomplete
- [x] yesterday complete @done(2020-06-01)
""" )
