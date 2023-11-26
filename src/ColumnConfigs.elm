module ColumnConfigs exposing
    ( ColumnConfigs
    , addTaskList
    , defaultForDateBoard
    , empty
    , fromList
    )

import Column exposing (Column, PlacementResult)
import ColumnConfig exposing (ColumnConfig)
import ColumnConfig.Completed as CompletedColumnConfig exposing (CompletedConfig)
import ColumnConfig.Date as DateColumnConfig exposing (DateConfig)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedColumn)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfigs
    = WithCompleted (List ColumnConfig) CompletedConfig
    | WithoutCompleted (List ColumnConfig)



-- TEMP TO REMOVE


fromList : List ColumnConfig -> Int -> ColumnConfigs
fromList columnConfigs completedCount =
    if completedCount > 0 then
        WithCompleted columnConfigs (CompletedColumnConfig.init completedCount)

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
        (CompletedColumnConfig.init 10)


empty : ColumnConfigs
empty =
    WithoutCompleted []


addTaskList : ColumnConfigs -> TaskList -> List (Column TaskItem)
addTaskList columnConfigs taskList =
    taskList
        |> TaskList.foldl addTaskItem columnConfigs
        |> columns



-- PRIVATE


columns : ColumnConfigs -> List (Column TaskItem)
columns columnConfigs =
    case columnConfigs of
        WithCompleted nonCompletedConfigs completedConfig ->
            nonCompletedConfigs
                |> List.map ColumnConfig.asColumn

        WithoutCompleted nonCompletedConfigs ->
            nonCompletedConfigs
                |> List.map ColumnConfig.asColumn


addTaskItem : TaskItem -> ColumnConfigs -> ColumnConfigs
addTaskItem taskItem columnConfigs =
    case columnConfigs of
        WithCompleted nonCompletedConfigs completedConfig ->
            let
                ( newConfigs, allPlacementResults ) =
                    addWithPlacement taskItem nonCompletedConfigs
            in
            WithCompleted newConfigs (CompletedColumnConfig.addTaskItem allPlacementResults taskItem completedConfig)

        WithoutCompleted nonCompletedConfigs ->
            let
                ( newConfigs, allPlacementResults ) =
                    addWithPlacement taskItem nonCompletedConfigs
            in
            WithoutCompleted newConfigs


addWithPlacement : TaskItem -> List ColumnConfig -> ( List ColumnConfig, List PlacementResult )
addWithPlacement taskItem initialConfigs =
    let
        fn : ColumnConfig -> ( List ColumnConfig, List PlacementResult ) -> ( List ColumnConfig, List PlacementResult )
        fn thisConfig ( accumulatedConfigs, placementResults ) =
            ColumnConfig.addTaskItem taskItem thisConfig
                |> Tuple.mapFirst (\c -> c :: accumulatedConfigs)
                |> Tuple.mapSecond (\r -> r :: placementResults)
    in
    List.foldr fn ( [], [] ) initialConfigs
