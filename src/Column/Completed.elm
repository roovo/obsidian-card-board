module Column.Completed exposing
    ( CompletedColumn
    , addTaskItem
    , asColumn
    , init
    )

import Column exposing (Column, PlacementResult)
import ColumnNames exposing (ColumnNames)
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


init : TagBoardConfig -> ColumnNames -> CompletedColumn
init tagBoardConfig columnNames =
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
        shouldBeAdded =
            placementResults
                |> List.filter (\r -> r /= Column.DoesNotBelong)
                |> List.all (\r -> r == Column.CompletedInThisColumn)
                |> (&&) (not <| List.isEmpty placementResults)
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
        |> TaskList.removeTags (tagsToHide completedColumn)
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.reverse
        |> List.sortBy TaskItem.completedPosix
        |> List.reverse
        |> List.take (completedCount completedColumn)
        |> Column.init (isEnabled completedColumn) (name completedColumn)



-- PRIVATE


belongs : TaskItem -> Bool
belongs =
    not << TaskItem.hasTags


completedCount : CompletedColumn -> Int
completedCount =
    .completedCount << config


config : CompletedColumn -> Config
config (CompletedColumn c) =
    c


isCompleted : TaskItem -> Bool
isCompleted taskItem =
    TaskItem.isCompleted taskItem


isEnabled : CompletedColumn -> Bool
isEnabled =
    not << (==) 0 << completedCount


name : CompletedColumn -> String
name =
    .name << config


tagsToHide : CompletedColumn -> List String
tagsToHide =
    .tagsToHide << config
