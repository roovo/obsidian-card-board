module ColumnConfig.Completed exposing
    ( CompletedConfig
    , addTaskItem
    , init
    )

import Column exposing (PlacementResult)
import TaskItem exposing (TaskItem)



-- TYPES


type CompletedConfig
    = CompletedConfig Config


type alias Config =
    { name : String
    , limit : Int
    }



-- CONSTRUCTION


init : Int -> CompletedConfig
init limit =
    CompletedConfig { name = "Completed", limit = limit }



-- MANIPULATION


addTaskItem : List PlacementResult -> TaskItem -> CompletedConfig -> CompletedConfig
addTaskItem placementResults taskItem completedConfig =
    completedConfig
