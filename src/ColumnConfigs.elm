module ColumnConfigs exposing
    ( ColumnConfigs
    , addTaskList
    , defaultForDateBoard
    , empty
    , fromList
    , updateColumnNames
    )

import Column exposing (Column, PlacementResult)
import ColumnConfig exposing (ColumnConfig)
import ColumnConfig.Completed as CompletedColumn exposing (CompletedColumn)
import ColumnConfig.Dated as DatedColumn exposing (DatedColumn)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedColumn)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfigs
    = WithCompleted (List ColumnConfig) CompletedColumn
    | WithoutCompleted (List ColumnConfig)



-- TEMP TO REMOVE


fromList : List ColumnConfig -> Int -> ColumnConfigs
fromList columnConfigs completedCount =
    if completedCount > 0 then
        WithCompleted columnConfigs (CompletedColumn.init completedCount)

    else
        WithoutCompleted columnConfigs



-- CONSTRUCTION


defaultForDateBoard : ColumnConfigs
defaultForDateBoard =
    WithCompleted
        [ ColumnConfig.defaultUndated
        , ColumnConfig.todayColumn
        , ColumnConfig.tomorrowColumn
        , ColumnConfig.futureColumn
        ]
        (CompletedColumn.init 10)


empty : ColumnConfigs
empty =
    WithoutCompleted []



-- MODIFICATION


addTaskList : Date -> ColumnConfigs -> TaskList -> List (Column TaskItem)
addTaskList today columnConfigs taskList =
    taskList
        |> TaskList.foldl (addTaskItem today) columnConfigs
        |> columns


updateColumnNames : ColumnNames -> ColumnConfigs -> ColumnConfigs
updateColumnNames columnNames columnConfigs =
    case columnConfigs of
        WithCompleted nonCompletedConfigs completedConfig ->
            WithCompleted (List.map (ColumnConfig.updateName columnNames) nonCompletedConfigs)
                completedConfig

        WithoutCompleted nonCompletedConfigs ->
            WithoutCompleted (List.map (ColumnConfig.updateName columnNames) nonCompletedConfigs)



-- PRIVATE


columns : ColumnConfigs -> List (Column TaskItem)
columns columnConfigs =
    case columnConfigs of
        WithCompleted nonCompletedConfigs completedConfig ->
            nonCompletedConfigs
                |> List.map ColumnConfig.asColumn
                |> List.reverse
                |> List.append [ CompletedColumn.asColumn completedConfig ]
                |> List.reverse

        WithoutCompleted nonCompletedConfigs ->
            nonCompletedConfigs
                |> List.map ColumnConfig.asColumn


addTaskItem : Date -> TaskItem -> ColumnConfigs -> ColumnConfigs
addTaskItem today taskItem columnConfigs =
    case columnConfigs of
        WithCompleted nonCompletedConfigs completedConfig ->
            let
                ( newConfigs, allPlacementResults ) =
                    addWithPlacement today taskItem nonCompletedConfigs
            in
            WithCompleted newConfigs
                (CompletedColumn.addTaskItem allPlacementResults taskItem completedConfig)

        WithoutCompleted nonCompletedConfigs ->
            let
                ( newConfigs, allPlacementResults ) =
                    addWithPlacement today taskItem nonCompletedConfigs
            in
            WithoutCompleted newConfigs


addWithPlacement : Date -> TaskItem -> List ColumnConfig -> ( List ColumnConfig, List PlacementResult )
addWithPlacement today taskItem initialConfigs =
    let
        fn : ColumnConfig -> ( List ColumnConfig, List PlacementResult ) -> ( List ColumnConfig, List PlacementResult )
        fn thisConfig ( accumulatedConfigs, placementResults ) =
            ColumnConfig.addTaskItem today taskItem thisConfig
                |> Tuple.mapFirst (\c -> c :: accumulatedConfigs)
                |> Tuple.mapSecond (\r -> r :: placementResults)
    in
    List.foldr fn ( [], [] ) initialConfigs
