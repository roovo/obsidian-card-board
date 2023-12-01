module Column.Undated exposing
    ( UndatedColumn
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


type UndatedColumn
    = UndatedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , enabled : Bool
    , name : String
    }



-- CONSTRUCTION


init : String -> UndatedColumn
init name_ =
    UndatedColumn { collapsed = False, enabled = True, name = name_ } [] TaskList.empty



-- INFO


name : UndatedColumn -> String
name (UndatedColumn c _ _) =
    c.name


toList : UndatedColumn -> List TaskItem
toList (UndatedColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)


isCollapsed : UndatedColumn -> Bool
isCollapsed (UndatedColumn c _ _) =
    c.collapsed


tagsToHide : UndatedColumn -> List String
tagsToHide (UndatedColumn _ tth _) =
    tth



-- MODIFICATION


addTaskItem : TaskItem -> UndatedColumn -> ( UndatedColumn, PlacementResult )
addTaskItem taskItem ((UndatedColumn c tth tl) as undatedColumn) =
    if not <| TaskItem.isDated taskItem then
        if TaskItem.isCompleted taskItem then
            ( undatedColumn, PlacementResult.CompletedInThisColumn )

        else
            ( UndatedColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

    else
        ( undatedColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> UndatedColumn -> UndatedColumn
setTagsToHide tags (UndatedColumn c _ tl) =
    UndatedColumn c tags tl
