module ColumnConfig.Undated exposing
    ( UndatedColumn
    , addTaskItem
    , init
    , isCollapsed
    , name
    , taskList
    )

import ColumnNames exposing (ColumnNames)
import PlacementResult exposing (PlacementResult)
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


name : UndatedColumn -> String
name (UndatedColumn c _) =
    c.name


taskList : UndatedColumn -> TaskList
taskList (UndatedColumn _ tl) =
    tl


isCollapsed : UndatedColumn -> Bool
isCollapsed (UndatedColumn c _) =
    c.collapsed



-- MODIFICATION


addTaskItem : TaskItem -> UndatedColumn -> ( UndatedColumn, PlacementResult )
addTaskItem taskItem ((UndatedColumn c tl) as undatedColumn) =
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( undatedColumn, PlacementResult.CompletedInThisColumn )

        else
            ( UndatedColumn c (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( undatedColumn, PlacementResult.DoesNotBelong )
