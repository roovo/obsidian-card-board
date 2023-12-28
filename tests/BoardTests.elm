module BoardTests exposing (suite)

import Board
import BoardConfig exposing (BoardConfig)
import Card
import Column
import Column.Completed as CompletedColumn
import Columns
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters = [ FilterHelpers.fileFilter "a" ]
                                    , filterPolarity = Filter.Allow
                                }
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters = [ FilterHelpers.fileFilter "a" ]
                                    , filterPolarity = Filter.Deny
                                }
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters = [ FilterHelpers.pathFilter "aa" ]
                                    , filterPolarity = Filter.Allow
                                }
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters = [ FilterHelpers.pathFilter "aa" ]
                                    , filterPolarity = Filter.Deny
                                }
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters = [ FilterHelpers.tagFilter "tag1" ]
                                    , filterPolarity = Filter.Allow
                                    , filterScope = Filter.Both
                                }
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters = [ FilterHelpers.tagFilter "tag1" ]
                                    , filterPolarity = Filter.Deny
                                    , filterScope = Filter.Both
                                }
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters =
                                        [ FilterHelpers.fileFilter "a"
                                        , FilterHelpers.pathFilter "aa"
                                        , FilterHelpers.tagFilter "tag1"
                                        , FilterHelpers.tagFilter "tag2"
                                        ]
                                    , filterPolarity = Filter.Allow
                                    , filterScope = Filter.Both
                                }
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
                            (BoardConfig.fromConfig
                                { exampleTagBoardConfig
                                    | filters =
                                        [ FilterHelpers.fileFilter "a"
                                        , FilterHelpers.pathFilter "aa"
                                        , FilterHelpers.tagFilter "tag1"
                                        , FilterHelpers.tagFilter "tag2"
                                        ]
                                    , filterPolarity = Filter.Deny
                                    , filterScope = Filter.Both
                                }
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
                Board.init "d1" (BoardConfig.fromConfig exampleTagBoardConfig) TaskList.empty
                    |> Board.id
                    |> Expect.equal "d1:Tag_Board_Name"
        ]



-- HELPERS


exampleTagBoardConfig : BoardConfig.Config
exampleTagBoardConfig =
    { columns =
        Columns.fromList
            [ Column.untagged "Untagged"
            , Column.otherTags "Others" [ "foo" ]
            , Column.namedTag "foo" "foo"
            , Column.completed <| CompletedColumn.init "Completed" 2 6
            ]
    , filters = [ FilterHelpers.pathFilter "a", FilterHelpers.pathFilter "b", FilterHelpers.tagFilter "t1", FilterHelpers.tagFilter "t2" ]
    , filterPolarity = Filter.Deny
    , filterScope = Filter.SubTasksOnly
    , showColumnTags = True
    , showFilteredTags = False
    , name = "Tag Board Name"
    }
