module Column.Completed exposing
    ( CompletedColumn
    , addTaskItem
    , asColumn
    , forDateBoard
    , forTagBoard
    )

import Column exposing (Column, PlacementResult)
import ColumnNames exposing (ColumnNames)
import DateBoardConfig exposing (DateBoardConfig)
import Filter
import TagBoardConfig exposing (TagBoardConfig)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type CompletedColumn
    = CompletedColumn Config


type alias Config =
    { completedCount : Int
    , name : String
    , taskList : TaskList
    , tagsToHide : List String
    }



-- CONSTRUCTION


forDateBoard : DateBoardConfig -> ColumnNames -> CompletedColumn
forDateBoard dateBoardConfig columnNames =
    let
        filterTagsToHide : List String
        filterTagsToHide =
            if dateBoardConfig.showFilteredTags then
                []

            else
                dateBoardConfig
                    |> .filters
                    |> List.filter (\f -> Filter.filterType f == "Tags")
                    |> List.map Filter.value
    in
    CompletedColumn
        { completedCount = dateBoardConfig.completedCount
        , name = ColumnNames.nameFor "completed" columnNames
        , taskList = TaskList.empty
        , tagsToHide = filterTagsToHide
        }


forTagBoard : TagBoardConfig -> ColumnNames -> CompletedColumn
forTagBoard tagBoardConfig columnNames =
    let
        columnTags : List String
        columnTags =
            tagBoardConfig
                |> .columns
                |> List.map .tag

        columnTagsToHide : List String
        columnTagsToHide =
            if tagBoardConfig.showColumnTags then
                []

            else
                columnTags

        filterTagsToHide : List String
        filterTagsToHide =
            if tagBoardConfig.showFilteredTags then
                []

            else
                tagBoardConfig
                    |> .filters
                    |> List.filter (\f -> Filter.filterType f == "Tags")
                    |> List.map Filter.value
    in
    CompletedColumn
        { completedCount = tagBoardConfig.completedCount
        , name = ColumnNames.nameFor "completed" columnNames
        , taskList = TaskList.empty
        , tagsToHide = columnTagsToHide ++ filterTagsToHide
        }


addTaskItem : List PlacementResult -> TaskItem -> CompletedColumn -> CompletedColumn
addTaskItem placementResults taskItem ((CompletedColumn c) as completedColumn) =
    let
        filteredPlacements : List PlacementResult
        filteredPlacements =
            placementResults
                |> List.filter (\r -> r /= Column.DoesNotBelong)

        shouldBeAdded : Bool
        shouldBeAdded =
            filteredPlacements
                |> List.all (\r -> r == Column.CompletedInThisColumn)
                |> (&&) (not <| List.isEmpty filteredPlacements)
    in
    if shouldBeAdded then
        CompletedColumn { c | taskList = TaskList.add taskItem c.taskList }

    else
        completedColumn



-- INFO


asColumn : CompletedColumn -> Column TaskItem
asColumn completedColumn =
    config completedColumn
        |> .taskList
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.reverse
        |> List.sortBy TaskItem.completedPosix
        |> List.reverse
        |> List.take (completedCount completedColumn)
        |> Column.init (isEnabled completedColumn) (name completedColumn) (tagsToHide completedColumn)



-- PRIVATE


completedCount : CompletedColumn -> Int
completedCount =
    .completedCount << config


config : CompletedColumn -> Config
config (CompletedColumn c) =
    c


isEnabled : CompletedColumn -> Bool
isEnabled =
    not << (==) 0 << completedCount


name : CompletedColumn -> String
name =
    .name << config


tagsToHide : CompletedColumn -> List String
tagsToHide =
    .tagsToHide << config
