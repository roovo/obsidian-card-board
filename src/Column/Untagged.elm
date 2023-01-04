module Column.Untagged exposing
    ( UntaggedColumn
    , addTaskItem
    , init
    , isEnabled
    , name
    , taskList
    )

import Column
import ColumnNames exposing (ColumnNames)
import TagBoard
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


init : TagBoard.Config -> ColumnNames -> UntaggedColumn
init tagboardConfig columnNames =
    UntaggedColumn
        { enabled = tagboardConfig.includeUntagged
        , name = ColumnNames.nameFor "untagged" columnNames
        , taskList = TaskList.empty
        }


addTaskItem : TaskItem -> UntaggedColumn -> ( UntaggedColumn, Column.PlacementResult )
addTaskItem taskItem ((UntaggedColumn c) as untaggedColumn) =
    if belongs taskItem then
        if isCompleted taskItem then
            ( untaggedColumn, Column.CompletedInThisColumn )

        else
            ( UntaggedColumn { c | taskList = TaskList.add taskItem c.taskList }, Column.Placed )

    else
        ( untaggedColumn, Column.DoesNotBelong )



-- INFO


isEnabled : UntaggedColumn -> Bool
isEnabled =
    .enabled << config


name : UntaggedColumn -> String
name =
    .name << config


taskList : UntaggedColumn -> TaskList
taskList =
    .taskList << config



-- PRIVATE


belongs : TaskItem -> Bool
belongs =
    not << TaskItem.hasTags


isCompleted : TaskItem -> Bool
isCompleted taskItem =
    TaskItem.isCompleted taskItem


config : UntaggedColumn -> Config
config (UntaggedColumn c) =
    c
