module DateBoardTests exposing (suite)

import Column
import DateBoard
import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import TaskItem
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
                    |> List.map Column.name
                    |> Expect.equal [ "Today", "Tomorrow", "Future" ]
        , test "todaysItems are sorted by due date (then task title ascending)" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone defaultConfig
                    |> BoardHelpers.thingsInColumn "Today"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "another yesterday incomplete", "yesterday incomplete", "today incomplete" ]
        , test "tommorrowsItems are sorted by task title ascending" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone defaultConfig
                    |> BoardHelpers.thingsInColumn "Tomorrow"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a task for tomorrow", "tomorrow incomplete" ]
        , test "futureItems are sorted by due date ascending (then task title)" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone defaultConfig
                    |> BoardHelpers.thingsInColumn "Future"
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
                    |> List.map Column.name
                    |> Expect.equal [ "Today", "Tomorrow", "Future", "Completed" ]
        , test "completedItems are sorted by completion date desc (then task title asc)" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone { defaultConfig | completedCount = 99 }
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "more undated complete"
                        , "future complete"
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
                    |> List.map Column.name
                    |> Expect.equal [ "Undated", "Today", "Tomorrow", "Future" ]
        , test "undatedItems are sorted by title ascending" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> DateBoard.columns DateTimeHelpers.nowWithZone { defaultConfig | includeUndated = True }
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "an undated incomplete"
                        , "incomplete with cTag"
                        , "invalid date incomplete"
                        , "more undated incomplete"
                        , "more undated incomplete with cTag"
                        , "untagged incomplete"
                        ]
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

        -- , test "builds the correct tsType" <|
        --     \() ->
        --         ""
        --             |> DecodeHelpers.runDecoder DateBoard.configDecoder
        --             |> .tsType
        --             |> Expect.equal ""
        ]



-- HELPERS


defaultConfig : DateBoard.Config
defaultConfig =
    BoardConfigHelpers.defaultDateBoardConfig


exampleConfig : DateBoard.Config
exampleConfig =
    BoardConfigHelpers.exampleDateBoardConfig
