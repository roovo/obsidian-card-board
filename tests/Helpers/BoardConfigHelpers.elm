module Helpers.BoardConfigHelpers exposing
    ( defaultDateBoardConfig
    , defaultTagBoardConfig
    , exampleBoardConfig
    , exampleDateBoardConfig
    , exampleTagBoardConfig
    )

import BoardConfig exposing (BoardConfig)
import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Columns
import DefaultColumnNames
import Filter
import Helpers.FilterHelpers as FilterHelpers


defaultDateBoardConfig : BoardConfig
defaultDateBoardConfig =
    BoardConfig.BoardConfig
        { columns =
            Columns.fromList
                [ Column.dated <| DatedColumn.init "Today" (DatedColumn.Before 1)
                , Column.dated <| DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 1, to = 1 })
                , Column.dated <| DatedColumn.init "Future" (DatedColumn.After 1)
                ]
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , showColumnTags = False
        , showFilteredTags = True
        , name = "Date Board Name"
        }


exampleBoardConfig : BoardConfig
exampleBoardConfig =
    exampleTagBoardConfig


exampleDateBoardConfig : BoardConfig
exampleDateBoardConfig =
    BoardConfig.BoardConfig
        { columns =
            Columns.fromList
                [ Column.undated "Undated"
                , Column.dated <| DatedColumn.init "Today" (DatedColumn.Before 1)
                , Column.dated <| DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 1, to = 1 })
                , Column.dated <| DatedColumn.init "Future" (DatedColumn.After 1)
                , Column.completed <| CompletedColumn.init "Completed" 3 12
                ]
        , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag1", FilterHelpers.tagFilter "tag2" ]
        , filterPolarity = Filter.Deny
        , filterScope = Filter.TopLevelOnly
        , showColumnTags = False
        , showFilteredTags = True
        , name = "Date Board Name"
        }


defaultTagBoardConfig : BoardConfig
defaultTagBoardConfig =
    BoardConfig.BoardConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , showColumnTags = True
        , showFilteredTags = True
        , name = "Tag Board Name"
        }


exampleTagBoardConfig : BoardConfig
exampleTagBoardConfig =
    BoardConfig.BoardConfig
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
