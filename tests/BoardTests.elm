module BoardTests exposing (suite)

import Board
import BoardConfig
import Card
import ColumnNames
import DateBoard
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskListHelpers as TaskListHelpers
import TagBoardConfig exposing (TagBoardConfig)
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
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters =
                                    [ FilterHelpers.fileFilter "gg/xx/yy.md"
                                    ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "an undated incomplete"
                        , "incomplete with cTag"
                        , "untagged incomplete"
                        ]
        , test "can filter tasks to NOT be from a given file" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , completedCount = 20
                                , filters =
                                    [ FilterHelpers.fileFilter "gg/xx/yy.md"
                                    , FilterHelpers.fileFilter "x"
                                    , FilterHelpers.fileFilter "b"
                                    , FilterHelpers.fileFilter "c"
                                    , FilterHelpers.fileFilter "d"
                                    ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "future complete"
                        , "yesterday complete"
                        , "invalid date complete"
                        ]
        , test "can filter tasks to be from a given path" <|
            \() ->
                TaskListHelpers.taskListFromFile "aa/bb/c.ext"
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters = [ FilterHelpers.pathFilter "aa/bb" ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal [ "c1" ]
        , test "can filter tasks to NOT be from a given path" <|
            \() ->
                TaskListHelpers.taskListFromFile "aa/bb/c.ext"
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters = [ FilterHelpers.pathFilter "aa/bb" ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "can filter tasks to have a given tag" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters = [ FilterHelpers.tagFilter "aTag" ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal [ "invalid date incomplete" ]
        , test "can filter tasks to NOT have a given tag" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters =
                                    [ FilterHelpers.tagFilter "aTag"
                                    , FilterHelpers.tagFilter "cTag"
                                    ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "an undated incomplete"
                        , "more undated incomplete"
                        , "untagged incomplete"
                        ]
        , test "filters tasks that are either in a file or path AND have one of the given tags" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters =
                                    [ FilterHelpers.fileFilter "f"
                                    , FilterHelpers.pathFilter "gg"
                                    , FilterHelpers.tagFilter "aTag"
                                    , FilterHelpers.tagFilter "bTag"
                                    ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "an undated incomplete"
                        , "invalid date incomplete"
                        ]
        , test "filters tasks that are NOT in the given files and paths AND DO NOT have one of the given tags" <|
            \() ->
                TaskListHelpers.exampleDateBoardTaskList
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.DateBoardConfig
                            { defaultDateBoardConfig
                                | includeUndated = True
                                , filters =
                                    [ FilterHelpers.fileFilter "f"
                                    , FilterHelpers.pathFilter "gg"
                                    , FilterHelpers.tagFilter "aTag"
                                    , FilterHelpers.tagFilter "bTag"
                                    ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal
                        [ "more undated incomplete with cTag" ]
        ]


columnsTagBoard : Test
columnsTagBoard =
    describe "columns - tagboard"
        [ test "can filter tasks to be from a given file" <|
            \() ->
                TaskListHelpers.exampleTagBoardTaskList
                    |> Board.init
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ FilterHelpers.fileFilter "a" ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
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
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ FilterHelpers.fileFilter "a" ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
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
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ FilterHelpers.pathFilter "aa" ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
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
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ FilterHelpers.pathFilter "aa" ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
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
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ FilterHelpers.tagFilter "tag1" ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
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
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters = [ FilterHelpers.tagFilter "tag1" ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
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
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters =
                                    [ FilterHelpers.fileFilter "a"
                                    , FilterHelpers.pathFilter "aa"
                                    , FilterHelpers.tagFilter "tag1"
                                    , FilterHelpers.tagFilter "tag2"
                                    ]
                                , filterPolarity = Filter.Allow
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
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
                        ColumnNames.default
                        (BoardConfig.TagBoardConfig
                            { defaultTagBoardConfig
                                | includeOthers = True
                                , filters =
                                    [ FilterHelpers.fileFilter "a"
                                    , FilterHelpers.pathFilter "aa"
                                    , FilterHelpers.tagFilter "tag1"
                                    , FilterHelpers.tagFilter "tag2"
                                    ]
                                , filterPolarity = Filter.Deny
                            }
                        )
                    |> Board.columns DateTimeHelpers.nowWithZone 0
                    |> BoardHelpers.thingsInColumn "Others"
                    |> List.map Card.taskItem
                    |> List.map TaskItem.title
                    |> Expect.equal [ "b.tag3" ]
        ]



-- HELPERS


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    BoardConfigHelpers.defaultDateBoardConfig


defaultTagBoardConfig : TagBoardConfig
defaultTagBoardConfig =
    BoardConfigHelpers.defaultTagBoardConfig
