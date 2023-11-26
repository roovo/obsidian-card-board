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
import ColumnConfig.Dated as DatedColumn exposing (DatedColumn)
import ColumnConfig.Undated as UndatedColumn exposing (UndatedColumn)
import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfig
    = Dated DatedColumn
    | Undated UndatedColumn



-- CONSTRUCTION


defaultUndated : ColumnConfig
defaultUndated =
    Undated UndatedColumn.init


futureColumn : ColumnConfig
futureColumn =
    Dated DatedColumn.future


todayColumn : ColumnConfig
todayColumn =
    Dated DatedColumn.forToday


tomorrowColumn : ColumnConfig
tomorrowColumn =
    Dated DatedColumn.tomorrow



-- MANIPULATION


addTaskItem : Date -> TaskItem -> ColumnConfig -> ( ColumnConfig, PlacementResult )
addTaskItem today taskItem columnConfig =
    case columnConfig of
        Dated datedColumn ->
            DatedColumn.addTaskItem today taskItem datedColumn
                |> Tuple.mapFirst Dated

        Undated undatedColumn ->
            UndatedColumn.addTaskItem taskItem undatedColumn
                |> Tuple.mapFirst Undated



-- INFO


asColumn : ColumnConfig -> Column TaskItem
asColumn columnConfig =
    case columnConfig of
        Dated datedColumn ->
            DatedColumn.asColumn datedColumn

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
