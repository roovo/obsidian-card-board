module ColumnConfig.Date exposing
    ( DateConfig
    , future
    , name
    , placementResult
    , today
    , tomorrow
    )

import Column exposing (PlacementResult)
import TaskItem exposing (TaskItem)



-- TYPES


type DateConfig
    = DateConfig Config


type alias Config =
    { name : String
    , range : RelativeDateRange
    }


type RelativeDateRange
    = Between Int Int
    | Before Int
    | After Int



-- CONSTRUCTION


today : DateConfig
today =
    DateConfig { name = "Today", range = Before 1 }


tomorrow : DateConfig
tomorrow =
    DateConfig { name = "Tomorrow", range = Between 1 1 }


future : DateConfig
future =
    DateConfig { name = "Future", range = After 1 }



-- INFO


placementResult : TaskItem -> DateConfig -> PlacementResult
placementResult taskItem config =
    Column.DoesNotBelong


name : DateConfig -> String
name (DateConfig c) =
    c.name
