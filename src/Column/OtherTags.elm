module Column.OtherTags exposing
    ( OtherTagsColumn
    , addTaskItem
    , asColumn
    , init
    )

import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import Filter
import TagBoardConfig exposing (TagBoardConfig)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type OtherTagsColumn
    = OtherTagsColumn Config


type alias Config =
    { enabled : Bool
    , name : String
    , taskList : TaskList
    , columnTags : List String
    , tagsToHide : List String
    }



-- CONSTRUCTION


init : TagBoardConfig -> ColumnNames -> OtherTagsColumn
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
    OtherTagsColumn
        { enabled = tagBoardConfig.includeOthers
        , name = ColumnNames.nameFor "others" columnNames
        , taskList = TaskList.empty
        , columnTags = columnTags
        , tagsToHide = columnTagsToHide ++ filterTagsToHide
        }


addTaskItem : TaskItem -> OtherTagsColumn -> ( OtherTagsColumn, Column.PlacementResult )
addTaskItem taskItem ((OtherTagsColumn c) as otherTagsColumn) =
    if isEnabled otherTagsColumn && belongs c.columnTags taskItem then
        if isCompleted c.columnTags taskItem then
            ( otherTagsColumn, Column.CompletedInThisColumn )

        else
            ( OtherTagsColumn { c | taskList = TaskList.add taskItem c.taskList }, Column.Placed )

    else
        ( otherTagsColumn, Column.DoesNotBelong )



-- INFO


asColumn : OtherTagsColumn -> Column TaskItem
asColumn otherTagsColumn =
    config otherTagsColumn
        |> .taskList
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init (isEnabled otherTagsColumn) (name otherTagsColumn) (tagsToHide otherTagsColumn)



-- PRIVATE


belongs : List String -> TaskItem -> Bool
belongs tags item =
    TaskItem.hasTaskWithTagOtherThanThese tags item


config : OtherTagsColumn -> Config
config (OtherTagsColumn c) =
    c


isCompleted : List String -> TaskItem -> Bool
isCompleted columnTags taskItem =
    TaskItem.isCompleted taskItem
        || (TaskItem.hasSubtasks taskItem
                && not (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese columnTags taskItem)
           )


isEnabled : OtherTagsColumn -> Bool
isEnabled =
    .enabled << config


name : OtherTagsColumn -> String
name =
    .name << config


tagsToHide : OtherTagsColumn -> List String
tagsToHide =
    .tagsToHide << config
