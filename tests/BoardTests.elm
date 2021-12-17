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
import TagBoard
import TaskItem
import Test exposing (..)


suite : Test
suite =
    concat
        [ columnsDateBoard
        , columnsTagBoard
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


columnsTagBoard : Test
columnsTagBoard =
    describe "columns - tagboard"
        [ test "can filter tasks to be from a given file" <|
            \() ->
                TaskListHelpers.exampleTagBoardTaskList
                    |> Board.init
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ Filter.FileFilter "a" ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Others"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "a.tag1"
                        , "a.tag2"
                        , "a.tag3"
                        ]
        , test "can filter tasks to be from a given path" <|
            \() ->
                TaskListHelpers.exampleTagBoardTaskList
                    |> Board.init
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ Filter.PathFilter "aa" ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Others"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "c.tag1"
                        , "c.tag2"
                        , "c.tag3"
                        ]
        , test "can filter tasks to have a given tag" <|
            \() ->
                TaskListHelpers.exampleTagBoardTaskList
                    |> Board.init
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ Filter.TagFilter "tag1" ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Others"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "a.tag1"
                        , "b.tag1"
                        , "c.tag1"
                        ]
        , test "filters tasks that are either in a file or path AND have one of the given tags" <|
            \() ->
                TaskListHelpers.exampleTagBoardTaskList
                    |> Board.init
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters =
                                    [ Filter.FileFilter "a"
                                    , Filter.PathFilter "aa"
                                    , Filter.TagFilter "tag1"
                                    , Filter.TagFilter "tag2"
                                    ]
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.cardsInColumn "Others"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "a.tag1"
                        , "a.tag2"
                        , "c.tag1"
                        , "c.tag2"
                        ]
        ]



-- HELPERS


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    BoardConfigHelpers.defaultDateBoardConfig


defaultTagBoardConfig : TagBoard.Config
defaultTagBoardConfig =
    BoardConfigHelpers.defaultTagBoardConfig
