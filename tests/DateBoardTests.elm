module DateBoardTests exposing (suite)

import DateBoard
import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import TaskItem exposing (TaskItem)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ columns
        , columnCompleted
        , columnUndated
        , encodeDecode
        ]


columns : Test
columns =
    describe "columns"
        [ test "default columns are just today tomorrow and future" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone defaultConfig
                    |> List.map Tuple.first
                    |> Expect.equal [ "Today", "Tomorrow", "Future" ]
        , test "todaysItems are sorted by due date (then task title ascending)" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone defaultConfig
                    |> BoardHelpers.tasksInColumn "Today"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "another yesterday incomplete", "yesterday incomplete", "today incomplete" ]
        , test "tommorrowsItems are sorted by task title ascending" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone defaultConfig
                    |> BoardHelpers.tasksInColumn "Tomorrow"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a task for tomorrow", "tomorrow incomplete" ]
        , test "futureItems are sorted by due date ascending (then task title)" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone defaultConfig
                    |> BoardHelpers.tasksInColumn "Future"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "future incomplete", "far future incomplete", "zapping into the future" ]
        ]


columnCompleted : Test
columnCompleted =
    describe "columnCompleted"
        [ test "a Completed column is appended if config sets includeCompleted" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone { defaultConfig | completedCount = 1 }
                    |> List.map Tuple.first
                    |> Expect.equal [ "Today", "Tomorrow", "Future", "Completed" ]
        , test "completedItems are sorted by completion date desc (then task title asc)" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone { defaultConfig | completedCount = 99 }
                    |> BoardHelpers.tasksInColumn "Completed"
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
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone { defaultConfig | includeUndated = True }
                    |> List.map Tuple.first
                    |> Expect.equal [ "Undated", "Today", "Tomorrow", "Future" ]
        , test "undatedItems are sorted by title ascending" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone { defaultConfig | includeUndated = True }
                    |> BoardHelpers.tasksInColumn "Undated"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "an undated incomplete", "invalid date incomplete" ]
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleConfig
                    |> TsEncode.runExample DateBoard.configEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder DateBoard.configDecoder
                    |> .decoded
                    |> Expect.equal (Ok exampleConfig)
        ]



-- HELPERS


defaultConfig : DateBoard.Config
defaultConfig =
    BoardConfigHelpers.defaultDateBoardConfig


exampleConfig : DateBoard.Config
exampleConfig =
    BoardConfigHelpers.exampleDateBoardConfig
