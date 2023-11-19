module Column.Untagged exposing
    ( UntaggedColumn
    , addTaskItem
    , asColumn
    , init
    )

import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import TagBoardConfig exposing (TagBoardConfig)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type UntaggedColumn
    = UntaggedColumn Config


type alias Config =
    { enabled : Bool
    , name : String
    , taskList : TaskList
    }



-- CONSTRUCTION


init : TagBoardConfig -> ColumnNames -> UntaggedColumn
init tagboardConfig columnNames =
    UntaggedColumn
        { enabled = tagboardConfig.includeUntagged
        , name = ColumnNames.nameFor "untagged" columnNames
        , taskList = TaskList.empty
        }


addTaskItem : TaskItem -> UntaggedColumn -> ( UntaggedColumn, Column.PlacementResult )
addTaskItem taskItem ((UntaggedColumn c) as untaggedColumn) =
    if isEnabled untaggedColumn && belongs taskItem then
        if isCompleted taskItem then
            ( untaggedColumn, Column.CompletedInThisColumn )

        else
            ( UntaggedColumn { c | taskList = TaskList.add taskItem c.taskList }, Column.Placed )

    else
        ( untaggedColumn, Column.DoesNotBelong )



-- INFO


asColumn : UntaggedColumn -> Column TaskItem
asColumn untaggedColumn =
    config untaggedColumn
        |> .taskList
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init (isEnabled untaggedColumn) (name untaggedColumn) []



-- PRIVATE


belongs : TaskItem -> Bool
belongs =
    not << TaskItem.hasTags


config : UntaggedColumn -> Config
config (UntaggedColumn c) =
    c


isCompleted : TaskItem -> Bool
isCompleted taskItem =
    TaskItem.isCompleted taskItem


isEnabled : UntaggedColumn -> Bool
isEnabled =
    .enabled << config


name : UntaggedColumn -> String
name =
    .name << config
