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
import DateBoardConfig exposing (DateBoardConfig)
import DefaultColumnNames
import Filter
import Helpers.FilterHelpers as FilterHelpers
import TagBoardConfig exposing (TagBoardConfig)


defaultDateBoardConfig : DateBoardConfig
defaultDateBoardConfig =
    { columns =
        Columns.fromList
            [ Column.dated <| DatedColumn.init "Today" (DatedColumn.Before 1)
            , Column.dated <| DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 1, to = 1 })
            , Column.dated <| DatedColumn.init "Future" (DatedColumn.After 1)
            ]
    , filters = []
    , filterPolarity = Filter.Allow
    , filterScope = Filter.Both
    , showFilteredTags = True
    , title = "Date Board Title"
    }


exampleBoardConfig : BoardConfig
exampleBoardConfig =
    BoardConfig.TagBoardConfig exampleTagBoardConfig


exampleDateBoardConfig : DateBoardConfig
exampleDateBoardConfig =
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
    , showFilteredTags = True
    , title = "Date Board Title"
    }


defaultTagBoardConfig : TagBoardConfig
defaultTagBoardConfig =
    { columns = Columns.empty
    , filters = []
    , filterPolarity = Filter.Allow
    , filterScope = Filter.Both
    , showColumnTags = True
    , showFilteredTags = True
    , title = "Tag Board Title"
    }


exampleTagBoardConfig : TagBoardConfig
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
    , title = "Tag Board Title"
    }
