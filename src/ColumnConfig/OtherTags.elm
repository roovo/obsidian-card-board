module ColumnConfig.OtherTags exposing
    ( OtherTagsColumn
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


type OtherTagsColumn
    = OtherTagsColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> List String -> OtherTagsColumn
init name_ otherTags_ =
    OtherTagsColumn { collapsed = False, name = name_ } otherTags_ TaskList.empty



-- INFO


asColumn : OtherTagsColumn -> Column TaskItem
asColumn ((OtherTagsColumn c _ tl) as namedTagColumn) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init True (name namedTagColumn) []
        |> Column.collapseState c.collapsed


name : OtherTagsColumn -> String
name (OtherTagsColumn c _ _) =
    c.name


taskList : OtherTagsColumn -> TaskList
taskList (OtherTagsColumn _ _ tl) =
    tl



-- MODIFICATION


addTaskItem : TaskItem -> OtherTagsColumn -> ( OtherTagsColumn, Column.PlacementResult )
addTaskItem taskItem ((OtherTagsColumn c ots tl) as namedTagColumn) =
    if belongs ots taskItem then
        if isCompleted ots taskItem then
            ( namedTagColumn, Column.CompletedInThisColumn )

        else
            ( OtherTagsColumn c ots (TaskList.add taskItem tl), Column.Placed )

    else
        ( namedTagColumn, Column.DoesNotBelong )



-- PRIVATE


belongs : List String -> TaskItem -> Bool
belongs tags item =
    TaskItem.hasTaskWithTagOtherThanThese tags item


isCompleted : List String -> TaskItem -> Bool
isCompleted columnTags taskItem =
    TaskItem.isCompleted taskItem
        || (TaskItem.hasSubtasks taskItem
                && not (TaskItem.hasAnyIncompleteSubtasksWithTagsOtherThanThese columnTags taskItem)
           )
