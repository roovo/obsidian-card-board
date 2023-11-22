module Helpers.BoardConfigHelpers exposing
    ( defaultDateBoardConfig
    , defaultTagBoardConfig
    , exampleBoardConfig
    , exampleDateBoardConfig
    , exampleTagBoardConfig
    )

import BoardConfig exposing (BoardConfig)
import CollapsedColumns
import DateBoardConfig exposing (DateBoardConfig)
import Filter
import Helpers.FilterHelpers as FilterHelpers
import TagBoardConfig exposing (TagBoardConfig)


defaultDateBoardConfig : DateBoardConfig
defaultDateBoardConfig =
    { collapsedColumns = CollapsedColumns.init
    , columnConfigs = []
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
    , columnConfigs = []
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
