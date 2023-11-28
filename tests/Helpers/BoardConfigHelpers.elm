module Helpers.BoardConfigHelpers exposing
    ( defaultDateBoardConfig
    , defaultTagBoardConfig
    , exampleBoardConfig
    , exampleDateBoardConfig
    , exampleTagBoardConfig
    )

import BoardConfig exposing (BoardConfig)
import CollapsedColumns
import ColumnConfig
import ColumnConfigs
import ColumnNames
import DateBoardConfig exposing (DateBoardConfig)
import Filter
import Helpers.FilterHelpers as FilterHelpers
import TagBoardConfig exposing (TagBoardConfig)


defaultDateBoardConfig : DateBoardConfig
defaultDateBoardConfig =
    { collapsedColumns = CollapsedColumns.init
    , columnConfigs =
        ColumnConfigs.fromList ColumnNames.default
            [ ColumnConfig.todayColumn
            , ColumnConfig.tomorrowColumn
            , ColumnConfig.futureColumn
            ]
            0
    , completedCount = 0
    , filters = []
    , filterPolarity = Filter.Allow
    , filterScope = Filter.Both
    , includeUndated = False
    , showFilteredTags = True
    , title = "Date Board Title"
    }


exampleBoardConfig : BoardConfig
exampleBoardConfig =
    BoardConfig.TagBoardConfig exampleTagBoardConfig


exampleDateBoardConfig : DateBoardConfig
exampleDateBoardConfig =
    { collapsedColumns = CollapsedColumns.init
    , columnConfigs =
        ColumnConfigs.fromList ColumnNames.default
            [ ColumnConfig.todayColumn
            , ColumnConfig.tomorrowColumn
            , ColumnConfig.futureColumn
            ]
            12
    , completedCount = 12
    , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag1", FilterHelpers.tagFilter "tag2" ]
    , filterPolarity = Filter.Deny
    , filterScope = Filter.TopLevelOnly
    , includeUndated = False
    , showFilteredTags = True
    , title = "Date Board Title"
    }


defaultTagBoardConfig : TagBoardConfig
defaultTagBoardConfig =
    { columns = []
    , columnConfigs = ColumnConfigs.fromList ColumnNames.default [] 0
    , showColumnTags = True
    , completedCount = 0
    , filters = []
    , filterPolarity = Filter.Allow
    , filterScope = Filter.Both
    , showFilteredTags = True
    , includeOthers = False
    , includeUntagged = False
    , title = "Tag Board Title"
    , collapsedColumns = CollapsedColumns.init
    }


exampleTagBoardConfig : TagBoardConfig
exampleTagBoardConfig =
    { columns = [ { tag = "foo", displayTitle = "bar" } ]
    , columnConfigs = ColumnConfigs.fromList ColumnNames.default [ ColumnConfig.defaultUntagged ] 6
    , showColumnTags = True
    , completedCount = 6
    , filters = [ FilterHelpers.pathFilter "a", FilterHelpers.pathFilter "b", FilterHelpers.tagFilter "t1", FilterHelpers.tagFilter "t2" ]
    , filterPolarity = Filter.Deny
    , filterScope = Filter.SubTasksOnly
    , showFilteredTags = False
    , includeOthers = False
    , includeUntagged = True
    , title = "Tag Board Title"
    , collapsedColumns = CollapsedColumns.init
    }
