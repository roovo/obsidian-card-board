module ColumnConfigs exposing
    ( ColumnConfigs
    , defaultForDateBoard
    , empty
    , fromList
    )

import ColumnConfig exposing (ColumnConfig)
import ColumnConfig.Completed as CompletedColumnConfig exposing (CompletedConfig)
import ColumnConfig.Date as DateColumnConfig exposing (DateConfig)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedConfig)
import TaskItem exposing (TaskItem)



-- TYPES


type ColumnConfigs
    = WithCompleted (List ColumnConfig) CompletedConfig
    | WithoutCompleted (List ColumnConfig)



-- TEMP TO REMOVE


fromList : List ColumnConfig -> Int -> ColumnConfigs
fromList columns completedCount =
    if completedCount > 0 then
        WithCompleted columns (CompletedColumnConfig.init completedCount)

    else
        WithoutCompleted columns



-- CONSTRUCTION


defaultForDateBoard : ColumnConfigs
defaultForDateBoard =
    WithCompleted
        [ ColumnConfig.defaultUndated
        , ColumnConfig.todayColumn
        , ColumnConfig.tomorrowColumn
        , ColumnConfig.futureColumn
        ]
        (CompletedColumnConfig.init 10)


empty : ColumnConfigs
empty =
    WithoutCompleted []



-- PRIVATE


addTaskItem : TaskItem -> ColumnConfigs -> ColumnConfigs
addTaskItem taskItem columnConfigs =
    case columnConfigs of
        WithCompleted nonCompletedConfig completedConfig ->
            WithCompleted (List.map (ColumnConfig.addTaskItem taskItem) nonCompletedConfig)
                (CompletedColumnConfig.addTaskItem taskItem completedConfig)

        WithoutCompleted nonCompletedConfig ->
            WithoutCompleted (List.map (ColumnConfig.addTaskItem taskItem) nonCompletedConfig)
