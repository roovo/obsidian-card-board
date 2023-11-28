module ColumnConfig.Undated exposing
    ( UndatedColumn
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


type UndatedColumn
    = UndatedColumn Config TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> UndatedColumn
init name_ =
    UndatedColumn { collapsed = False, name = name_ } TaskList.empty



-- INFO


asColumn : UndatedColumn -> Column TaskItem
asColumn ((UndatedColumn c tl) as undatedColumn) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> Column.init True (name undatedColumn) []
        |> Column.collapseState c.collapsed


name : UndatedColumn -> String
name (UndatedColumn c _) =
    c.name


taskList : UndatedColumn -> TaskList
taskList (UndatedColumn _ tl) =
    tl



-- MODIFICATION


addTaskItem : TaskItem -> UndatedColumn -> ( UndatedColumn, Column.PlacementResult )
addTaskItem taskItem ((UndatedColumn c tl) as undatedColumn) =
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( undatedColumn, Column.CompletedInThisColumn )

        else
            ( UndatedColumn c (TaskList.add taskItem tl), Column.Placed )

    else
        ( undatedColumn, Column.DoesNotBelong )
