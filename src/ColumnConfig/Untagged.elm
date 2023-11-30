module ColumnConfig.Untagged exposing
    ( UntaggedColumn
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


name : UntaggedColumn -> String
name (UntaggedColumn c _) =
    c.name


taskList : UntaggedColumn -> TaskList
taskList (UntaggedColumn _ tl) =
    tl


isCollapsed : UntaggedColumn -> Bool
isCollapsed (UntaggedColumn c _) =
    c.collapsed



-- MODIFICATION


addTaskItem : TaskItem -> UntaggedColumn -> ( UntaggedColumn, PlacementResult )
addTaskItem taskItem ((UntaggedColumn c tl) as untaggedColumn) =
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( untaggedColumn, PlacementResult.CompletedInThisColumn )

        else
            ( UntaggedColumn c (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( untaggedColumn, PlacementResult.DoesNotBelong )
