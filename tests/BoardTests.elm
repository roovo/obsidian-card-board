module BoardTests exposing (suite)

import Board
import BoardConfig
import Card
import DateBoard
import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import TaskItem
import Test exposing (..)


suite : Test
suite =
    concat
        [ columnsDateBoard
        ]


columnsDateBoard : Test
columnsDateBoard =
    describe "columns - dateboard"
        [ test "puts undated tasks in the undated column" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init (BoardConfig.DateBoardConfig { defaultDateBoardConfig | includeUndated = True })
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal [ "an undated incomplete", "invalid date incomplete" ]
        ]



-- HELPERS


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    BoardConfigHelpers.defaultDateBoardConfig
