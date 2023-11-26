module ColumnConfig exposing
    ( ColumnConfig
    , addTaskItem
    , asColumn
    , defaultUndated
    , futureColumn
    , todayColumn
    , tomorrowColumn
    )

import Column exposing (Column, PlacementResult)
import ColumnConfig.Completed as CompletedColumnConfig exposing (CompletedConfig)
import ColumnConfig.Date as DateColumnConfig exposing (DateConfig)
import ColumnConfig.Undated as UndatedColumn exposing (UndatedColumn)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfig
    = Dated DateConfig TaskList
    | Undated UndatedColumn



-- CONSTRUCTION


defaultUndated : ColumnConfig
defaultUndated =
    Undated UndatedColumn.init


futureColumn : ColumnConfig
futureColumn =
    Dated DateColumnConfig.future TaskList.empty


todayColumn : ColumnConfig
todayColumn =
    Dated DateColumnConfig.today TaskList.empty


tomorrowColumn : ColumnConfig
tomorrowColumn =
    Dated DateColumnConfig.tomorrow TaskList.empty



-- MANIPULATION


addTaskItem : TaskItem -> ColumnConfig -> ( ColumnConfig, PlacementResult )
addTaskItem taskItem columnConfig =
    case columnConfig of
        Dated config taskList ->
            let
                placementResult =
                    DateColumnConfig.placementResult taskItem config
            in
            ( Dated config (placeTask placementResult taskItem taskList)
            , placementResult
            )

        Undated undatedColumn ->
            UndatedColumn.addTaskItem taskItem undatedColumn
                |> Tuple.mapFirst Undated



-- INFO


asColumn : ColumnConfig -> Column TaskItem
asColumn columnConfig =
    case columnConfig of
        Dated config taskList ->
            Column.init True (DateColumnConfig.name config) [] []

        Undated undatedColumn ->
            UndatedColumn.asColumn undatedColumn



-- PRIVATE


placeTask : PlacementResult -> TaskItem -> TaskList -> TaskList
placeTask pr item list =
    case pr of
        Column.CompletedInThisColumn ->
            list

        Column.DoesNotBelong ->
            list

        Column.Placed ->
            list
