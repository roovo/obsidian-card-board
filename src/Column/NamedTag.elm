module Column.NamedTag exposing
    ( NamedTagColumn
    , addTaskItem
    , init
    , isCollapsed
    , name
    , setTagsToHide
    , tagsToHide
    , toList
    )

import ColumnNames exposing (ColumnNames)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type NamedTagColumn
    = NamedTagColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    , tag : String
    }



-- CONSTRUCTION


init : String -> String -> NamedTagColumn
init name_ tag_ =
    NamedTagColumn { collapsed = False, name = name_, tag = tag_ } [] TaskList.empty



-- INFO


isCollapsed : NamedTagColumn -> Bool
isCollapsed (NamedTagColumn c _ _) =
    c.collapsed


name : NamedTagColumn -> String
name (NamedTagColumn c _ _) =
    c.name


toList : NamedTagColumn -> List TaskItem
toList (NamedTagColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


tagsToHide : NamedTagColumn -> List String
tagsToHide (NamedTagColumn _ tth _) =
    tth



-- MODIFICATION


addTaskItem : TaskItem -> NamedTagColumn -> ( NamedTagColumn, PlacementResult )
addTaskItem taskItem ((NamedTagColumn c tth tl) as namedTagColumn) =
    if belongs c.tag taskItem then
        if TaskItem.isCompleted taskItem then
            ( namedTagColumn, PlacementResult.CompletedInThisColumn )

        else
            ( NamedTagColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( namedTagColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> NamedTagColumn -> NamedTagColumn
setTagsToHide tags (NamedTagColumn c _ tl) =
    NamedTagColumn c tags tl



-- PRIVATE


belongs : String -> TaskItem -> Bool
belongs t =
    TaskItem.hasThisTag t
