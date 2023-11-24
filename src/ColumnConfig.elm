module ColumnConfig exposing
    ( ColumnConfig
    , addTaskItem
    , defaultUndated
    , futureColumn
    , todayColumn
    , tomorrowColumn
    )

import ColumnConfig.Completed as CompletedColumnConfig exposing (CompletedConfig)
import ColumnConfig.Date as DateColumnConfig exposing (DateConfig)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedConfig)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfig
    = Date DateConfig TaskList
    | Undated UndatedConfig TaskList



-- CONSTRUCTION


defaultUndated : ColumnConfig
defaultUndated =
    Undated UndatedColumnConfig.init TaskList.empty


futureColumn : ColumnConfig
futureColumn =
    Date DateColumnConfig.future TaskList.empty


todayColumn : ColumnConfig
todayColumn =
    Date DateColumnConfig.today TaskList.empty


tomorrowColumn : ColumnConfig
tomorrowColumn =
    Date DateColumnConfig.tomorrow TaskList.empty



-- MANIPULATION


addTaskItem : TaskItem -> ColumnConfig -> ColumnConfig
addTaskItem taskItem columnConfig =
    columnConfig
