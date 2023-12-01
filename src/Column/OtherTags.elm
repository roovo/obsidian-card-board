module Column.OtherTags exposing
    ( OtherTagsColumn
    , addTaskItem
    , init
    , isCollapsed
    , name
    , setTagsToHide
    , toList
    )

import ColumnNames exposing (ColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type OtherTagsColumn
    = OtherTagsColumn Config (List String) (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> List String -> OtherTagsColumn
init name_ otherTags_ =
    OtherTagsColumn { collapsed = False, name = name_ } otherTags_ [] TaskList.empty



-- INFO


isCollapsed : OtherTagsColumn -> Bool
isCollapsed (OtherTagsColumn c _ _ _) =
    c.collapsed


name : OtherTagsColumn -> String
name (OtherTagsColumn c _ _ _) =
    c.name


toList : OtherTagsColumn -> List TaskItem
toList (OtherTagsColumn _ _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie



-- MODIFICATION


addTaskItem : TaskItem -> OtherTagsColumn -> ( OtherTagsColumn, PlacementResult )
addTaskItem taskItem ((OtherTagsColumn c ots tth tl) as namedTagColumn) =
    if belongs ots taskItem then
        if isCompleted ots taskItem then
            ( namedTagColumn, PlacementResult.CompletedInThisColumn )

        else
            ( OtherTagsColumn c ots tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( namedTagColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> OtherTagsColumn -> OtherTagsColumn
setTagsToHide tags (OtherTagsColumn c ots _ tl) =
    OtherTagsColumn c ots tags tl



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
