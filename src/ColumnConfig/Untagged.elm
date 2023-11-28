module ColumnConfig.Untagged exposing
    ( UntaggedColumn
    , addTaskItem
    , asColumn
    , init
    , name
    )

import Column exposing (Column, PlacementResult)
import ColumnNames exposing (ColumnNames)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type UntaggedColumn
    = UntaggedColumn Config TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> UntaggedColumn
init name_ =
    UntaggedColumn { collapsed = False, name = name_ } TaskList.empty



-- INFO


asColumn : UntaggedColumn -> Column TaskItem
asColumn ((UntaggedColumn c tl) as untaggedColumn) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> Column.init True (name untaggedColumn) []
        |> Column.collapseState c.collapsed


name : UntaggedColumn -> String
name (UntaggedColumn c _) =
    c.name


taskList : UntaggedColumn -> TaskList
taskList (UntaggedColumn _ tl) =
    tl



-- MODIFICATION


addTaskItem : TaskItem -> UntaggedColumn -> ( UntaggedColumn, Column.PlacementResult )
addTaskItem taskItem ((UntaggedColumn c tl) as untaggedColumn) =
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( untaggedColumn, Column.CompletedInThisColumn )

        else
            ( UntaggedColumn c (TaskList.add taskItem tl), Column.Placed )

    else
        ( untaggedColumn, Column.DoesNotBelong )
