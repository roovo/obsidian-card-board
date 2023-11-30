module ColumnConfigs exposing
    ( ColumnConfigs
    , addTaskList
    , empty
    , fromList
    , toList
    )

import ColumnConfig exposing (ColumnConfig)
import ColumnConfig.Completed as CompletedColumn exposing (CompletedColumn)
import ColumnConfig.Dated as DatedColumn exposing (DatedColumn)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedColumn)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfigs
    = WithCompleted (List ColumnConfig) CompletedColumn
    | WithoutCompleted (List ColumnConfig)



-- TEMP TO REMOVE


fromList : ColumnNames -> List ColumnConfig -> Int -> ColumnConfigs
fromList columnNames columnConfigs completedCount =
    if completedCount > 0 then
        WithCompleted
            columnConfigs
            (CompletedColumn.init
                (ColumnNames.nameFor "completed" columnNames)
                (List.length columnConfigs)
                completedCount
            )

    else
        WithoutCompleted columnConfigs



-- CONSTRUCTION


empty : ColumnConfigs
empty =
    WithoutCompleted []



-- MODIFICATION


addTaskList : Date -> ColumnConfigs -> TaskList -> ColumnConfigs
addTaskList today columnConfigs taskList =
    taskList
        |> TaskList.foldl (addTaskItem today) columnConfigs


toList : ColumnConfigs -> List ColumnConfig
toList columnConfigs =
    case columnConfigs of
        WithCompleted nonCompletedConfigs completedConfig ->
            nonCompletedConfigs
                |> insert (CompletedColumn.index completedConfig) (ColumnConfig.completed completedConfig)

        WithoutCompleted nonCompletedConfigs ->
            nonCompletedConfigs



-- PRIVATE


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


insert : Int -> a -> List a -> List a
insert i value list =
    List.take i list ++ [ value ] ++ List.drop i list
