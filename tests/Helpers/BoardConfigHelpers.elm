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
import TagBoard


defaultDateBoardConfig : DateBoard.Config
defaultDateBoardConfig =
    { completedCount = 0
    , filters = []
    , filterPolarity = Filter.Allow
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
    , includeUndated = False
    , title = "Date Board Title"
    }


defaultTagBoardConfig : TagBoard.Config
defaultTagBoardConfig =
    { columns = []
    , completedCount = 0
    , filters = []
    , filterPolarity = Filter.Allow
    , includeOthers = False
    , includeUntagged = False
    , title = "Tag Board Title"
    }


exampleTagBoardConfig : TagBoard.Config
exampleTagBoardConfig =
    { columns = [ { tag = "foo", displayTitle = "bar" } ]
    , completedCount = 6
    , filters = [ FilterHelpers.pathFilter "a", FilterHelpers.pathFilter "b", FilterHelpers.tagFilter "t1", FilterHelpers.tagFilter "t2" ]
    , filterPolarity = Filter.Deny
    , includeOthers = False
    , includeUntagged = True
    , title = "Tag Board Title"
    }
