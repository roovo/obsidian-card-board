module BoardTests exposing (suite)

import Board
import BoardConfig
import Card
import DateBoard
import Expect
import Filter
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
        [ test "can filter tasks to be from a given file" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters = [ Filter.FileFilter "gg/xx/yy.md" ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "an undated incomplete"
                        , "incomplete with cTag"
                        , "untagged incomplete"
                        ]
        , test "can filter tasks to be from a given path" <|
            \() ->
                TaskListHelpers.taskListFromFile "aa/bb/c.ext"
                    |> Board.init
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters = [ Filter.PathFilter "aa/bb" ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c1" ]
        , test "can filter tasks to have a given tag" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters = [ Filter.TagFilter "aTag" ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal [ "invalid date incomplete" ]
        , test "filters tasks that are either in a file or path AND have one of the given tags" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters =
                                    [ Filter.FileFilter "f"
                                    , Filter.PathFilter "gg"
                                    , Filter.TagFilter "aTag"
                                    , Filter.TagFilter "bTag"
                                    ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "an undated incomplete"
                        , "invalid date incomplete"
                        ]
        ]



-- HELPERS


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    BoardConfigHelpers.defaultDateBoardConfig
