module Column.Untagged exposing
    ( UntaggedColumn
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


type UntaggedColumn
    = UntaggedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , enabled : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> UntaggedColumn
init name_ =
    UntaggedColumn { collapsed = False, enabled = True, name = name_ } [] TaskList.empty



-- INFO


name : UntaggedColumn -> String
name (UntaggedColumn c _ _) =
    c.name


toList : UntaggedColumn -> List TaskItem
toList (UntaggedColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


isCollapsed : UntaggedColumn -> Bool
isCollapsed (UntaggedColumn c _ _) =
    c.collapsed


tagsToHide : UntaggedColumn -> List String
tagsToHide (UntaggedColumn _ tth _) =
    tth



-- MODIFICATION


addTaskItem : TaskItem -> UntaggedColumn -> ( UntaggedColumn, PlacementResult )
addTaskItem taskItem ((UntaggedColumn c tth tl) as untaggedColumn) =
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( untaggedColumn, PlacementResult.CompletedInThisColumn )

        else
            ( UntaggedColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( untaggedColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> UntaggedColumn -> UntaggedColumn
setTagsToHide tags (UntaggedColumn c _ tl) =
    UntaggedColumn c tags tl
