module ColumnConfig.NamedTag exposing
    ( NamedTagColumn
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


type NamedTagColumn
    = NamedTagColumn Config TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    , tag : String
    }



-- CONSTRUCTION


init : String -> String -> NamedTagColumn
init name_ tag_ =
    NamedTagColumn { collapsed = False, name = name_, tag = tag_ } TaskList.empty



-- INFO


isCollapsed : NamedTagColumn -> Bool
isCollapsed (NamedTagColumn c _) =
    c.collapsed


name : NamedTagColumn -> String
name (NamedTagColumn c _) =
    c.name


taskList : NamedTagColumn -> TaskList
taskList (NamedTagColumn _ tl) =
    tl



-- MODIFICATION


addTaskItem : TaskItem -> NamedTagColumn -> ( NamedTagColumn, PlacementResult )
addTaskItem taskItem ((NamedTagColumn c tl) as namedTagColumn) =
    if belongs c.tag taskItem then
        if TaskItem.isCompleted taskItem then
            ( namedTagColumn, PlacementResult.CompletedInThisColumn )

        else
            ( NamedTagColumn c (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( namedTagColumn, PlacementResult.DoesNotBelong )



-- PRIVATE


belongs : String -> TaskItem -> Bool
belongs t =
    TaskItem.hasThisTag t
