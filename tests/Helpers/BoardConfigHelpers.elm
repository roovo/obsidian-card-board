module Helpers.BoardConfigHelpers exposing
    ( defaultDateBoardConfig
    , defaultTagBoardConfig
    , exampleBoardConfig
    , exampleDateBoardConfig
    , exampleTagBoardConfig
    )

import BoardConfig exposing (BoardConfig)
import DateBoard
import Filter
import Helpers.FilterHelpers as FilterHelpers
import TagBoardConfig exposing (TagBoardConfig)


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    { completedCount = 0
    , filters = []
    , filterPolarity = Filter.Allow
    , showFilteredTags = True
    , includeUndated = False
    , title = "Date Board Title"
    }


exampleBoardConfig : BoardConfig
exampleBoardConfig =
    BoardConfig.TagBoardConfig exampleTagBoardConfig


exampleDateBoardConfig : DateBoard.Config
exampleDateBoardConfig =
    { completedCount = 12
    , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag1", FilterHelpers.tagFilter "tag2" ]
    , filterPolarity = Filter.Deny
    , showFilteredTags = True
    , includeUndated = False
    , title = "Date Board Title"
    }


defaultTagBoardConfig : TagBoardConfig
defaultTagBoardConfig =
    { columns = []
    , showColumnTags = True
    , completedCount = 0
    , filters = []
    , filterPolarity = Filter.Allow
    , showFilteredTags = True
    , includeOthers = False
    , includeUntagged = False
    , title = "Tag Board Title"
    }


exampleTagBoardConfig : TagBoardConfig
exampleTagBoardConfig =
    { columns = [ { tag = "foo", displayTitle = "bar" } ]
    , showColumnTags = True
    , completedCount = 6
    , filters = [ FilterHelpers.pathFilter "a", FilterHelpers.pathFilter "b", FilterHelpers.tagFilter "t1", FilterHelpers.tagFilter "t2" ]
    , filterPolarity = Filter.Deny
    , showFilteredTags = False
    , includeOthers = False
    , includeUntagged = True
    , title = "Tag Board Title"
    }
