module BoardTests exposing (suite)

import Board
import BoardConfig exposing (BoardConfig)
import Card
import Column
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import TaskItem
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ columns
        , id
        ]


columns : Test
columns =
    describe "columns"
        [ describe "filtering"
            [ test "can filter tasks to be from a given file" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters [ FilterHelpers.fileFilter "a" ]
                                |> BoardConfig.updateFilterPolarity "Allow"
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal
                            [ "a.tag1"
                            , "a.tag2"
                            , "a.tag3"
                            ]
            , test "can filter tasks to NOT be from a given file" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters [ FilterHelpers.fileFilter "a" ]
                                |> BoardConfig.updateFilterPolarity "Deny"
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal
                            [ "b.tag1"
                            , "b.tag2"
                            , "b.tag3"
                            , "c.tag1"
                            , "c.tag2"
                            , "c.tag3"
                            ]
            , test "can filter tasks to be from a given path" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters [ FilterHelpers.pathFilter "aa" ]
                                |> BoardConfig.updateFilterPolarity "Allow"
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal
                            [ "c.tag1"
                            , "c.tag2"
                            , "c.tag3"
                            ]
            , test "can filter tasks to NOT be from a given path" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters [ FilterHelpers.pathFilter "aa" ]
                                |> BoardConfig.updateFilterPolarity "Deny"
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal
                            [ "a.tag1"
                            , "a.tag2"
                            , "a.tag3"
                            , "b.tag1"
                            , "b.tag2"
                            , "b.tag3"
                            ]
            , test "can filter tasks to have a given tag" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters [ FilterHelpers.tagFilter "tag1" ]
                                |> BoardConfig.updateFilterPolarity "Allow"
                                |> BoardConfig.updateFilterScope Filter.Both
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal
                            [ "a.tag1"
                            , "b.tag1"
                            , "c.tag1"
                            ]
            , test "can filter tasks to NOT have a given tag" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters [ FilterHelpers.tagFilter "tag1" ]
                                |> BoardConfig.updateFilterPolarity "Deny"
                                |> BoardConfig.updateFilterScope Filter.Both
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal
                            [ "a.tag2"
                            , "a.tag3"
                            , "b.tag2"
                            , "b.tag3"
                            , "c.tag2"
                            , "c.tag3"
                            ]
            , test "filters tasks that are either in a file or path AND have one of the given tags" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters
                                    [ FilterHelpers.fileFilter "a"
                                    , FilterHelpers.pathFilter "aa"
                                    , FilterHelpers.tagFilter "tag1"
                                    , FilterHelpers.tagFilter "tag2"
                                    ]
                                |> BoardConfig.updateFilterPolarity "Allow"
                                |> BoardConfig.updateFilterScope Filter.Both
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal
                            [ "a.tag1"
                            , "a.tag2"
                            , "c.tag1"
                            , "c.tag2"
                            ]
            , test "filters tasks that are NOT in any of the given file or path AND DO NOT have one of the given tags" <|
                \() ->
                    TaskListHelpers.exampleTagBoardTaskList
                        |> Board.init
                            "d1"
                            (exampleTagBoardConfig
                                |> BoardConfig.updateFilters
                                    [ FilterHelpers.fileFilter "a"
                                    , FilterHelpers.pathFilter "aa"
                                    , FilterHelpers.tagFilter "tag1"
                                    , FilterHelpers.tagFilter "tag2"
                                    ]
                                |> BoardConfig.updateFilterPolarity "Deny"
                                |> BoardConfig.updateFilterScope Filter.Both
                            )
                        |> Board.columns False DateTimeHelpers.todayDate
                        |> BoardHelpers.thingsInColumn "Others"
                        |> List.map Card.taskItem
                        |> List.map TaskItem.title
                        |> Expect.equal [ "b.tag3" ]
            ]
        ]


id : Test
id =
    describe "id"
        [ test "is made up from the prefix and the board name" <|
            \() ->
                Board.init "d1" exampleTagBoardConfig TaskList.empty
                    |> Board.id
                    |> Expect.equal "d1:Tag_Board_Name"
        ]



-- HELPERS


exampleTagBoardConfig : BoardConfig
exampleTagBoardConfig =
    BoardConfigHelpers.exampleTagBoardConfig
