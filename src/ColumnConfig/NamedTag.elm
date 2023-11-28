module ColumnConfig.NamedTag exposing
    ( NamedTagColumn
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


asColumn : NamedTagColumn -> Column TaskItem
asColumn ((NamedTagColumn c tl) as namedTagColumn) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init True (name namedTagColumn) []
        |> Column.collapseState c.collapsed


name : NamedTagColumn -> String
name (NamedTagColumn c _) =
    c.name


taskList : NamedTagColumn -> TaskList
taskList (NamedTagColumn _ tl) =
    tl



-- MODIFICATION


addTaskItem : TaskItem -> NamedTagColumn -> ( NamedTagColumn, Column.PlacementResult )
addTaskItem taskItem ((NamedTagColumn c tl) as namedTagColumn) =
    if belongs c.tag taskItem then
        if TaskItem.isCompleted taskItem then
            ( namedTagColumn, Column.CompletedInThisColumn )

        else
            ( NamedTagColumn c (TaskList.add taskItem tl), Column.Placed )

    else
        ( namedTagColumn, Column.DoesNotBelong )



-- PRIVATE


belongs : String -> TaskItem -> Bool
belongs t =
    TaskItem.hasThisTag t
