module Column.NamedTag exposing
    ( NamedTagColumn
    , addTaskItem
    , asColumn
    , init
    )

import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import Filter
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type NamedTagColumn
    = NamedTagColumn Config


type alias Config =
    { name : String
    , tag : String
    , taskList : TaskList
    , tagsToHide : List String
    }



-- CONSTRUCTION


init : TagBoard.Config -> TagBoard.ColumnConfig -> NamedTagColumn
init tagBoardConfig columnConfig =
    let
        columnTagsToHide : List String
        columnTagsToHide =
            if tagBoardConfig.showColumnTags then
                []

            else
                tagBoardConfig
                    |> .columns
                    |> List.map .tag

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
    NamedTagColumn
        { name = columnConfig.displayTitle
        , tag = columnConfig.tag
        , taskList = TaskList.empty
        , tagsToHide = columnTagsToHide ++ filterTagsToHide
        }


addTaskItem : TaskItem -> NamedTagColumn -> ( NamedTagColumn, Column.PlacementResult )
addTaskItem taskItem ((NamedTagColumn c) as namedTagColumn) =
    let
        columnTag =
            tag namedTagColumn
    in
    if belongs columnTag taskItem then
        if isCompleted columnTag taskItem then
            ( namedTagColumn, Column.CompletedInThisColumn )

        else
            ( NamedTagColumn { c | taskList = TaskList.add taskItem c.taskList }, Column.Placed )

    else
        ( namedTagColumn, Column.DoesNotBelong )



-- INFO


asColumn : NamedTagColumn -> Column TaskItem
asColumn namedTagColumn =
    config namedTagColumn
        |> .taskList
        |> TaskList.removeTags (tagsToRemove namedTagColumn)
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init True (name namedTagColumn)



-- PRIVATE


belongs : String -> TaskItem -> Bool
belongs t =
    TaskItem.hasThisTag t


config : NamedTagColumn -> Config
config (NamedTagColumn c) =
    c


isCompleted : String -> TaskItem -> Bool
isCompleted t taskItem =
    TaskItem.isCompleted taskItem
        || (TaskItem.hasSubtasks taskItem && TaskItem.allSubtasksWithMatchingTagCompleted t taskItem)


name : NamedTagColumn -> String
name =
    .name << config


tag : NamedTagColumn -> String
tag =
    .tag << config


tagsToRemove : NamedTagColumn -> List String
tagsToRemove =
    .tagsToHide << config
