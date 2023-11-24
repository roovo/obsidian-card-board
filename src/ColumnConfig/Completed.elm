module ColumnConfig.Completed exposing
    ( CompletedConfig
    , addTaskItem
    , init
    )

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


addTaskItem : TaskItem -> CompletedConfig -> CompletedConfig
addTaskItem taskItem completedConfig =
    completedConfig
