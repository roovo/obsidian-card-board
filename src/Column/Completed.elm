module Column.Completed exposing
    ( CompletedColumn
    , addTaskItem
    , index
    , init
    , isCollapsed
    , name
    , setTagsToHide
    , tagsToHide
    , toList
    )

import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type CompletedColumn
    = CompletedColumn Config (List String) TaskList


type alias Config =
    { collapsed : Bool
    , index : Int
    , limit : Int
    , name : String
    }



-- CONSTRUCTION


init : String -> Int -> Int -> CompletedColumn
init name_ index_ limit_ =
    CompletedColumn { collapsed = False, index = index_, limit = limit_, name = name_ } [] TaskList.empty



-- INFO


index : CompletedColumn -> Int
index (CompletedColumn c _ _) =
    c.index


isCollapsed : CompletedColumn -> Bool
isCollapsed (CompletedColumn c _ _) =
    c.collapsed


name : CompletedColumn -> String
name (CompletedColumn c _ _) =
    c.name


tagsToHide : CompletedColumn -> List String
tagsToHide (CompletedColumn _ tth _) =
    tth


toList : CompletedColumn -> List TaskItem
toList (CompletedColumn c _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.reverse
        |> List.sortBy TaskItem.completedPosix
        |> List.reverse
        |> List.take c.limit



-- MANIPULATION


addTaskItem : List PlacementResult -> TaskItem -> CompletedColumn -> CompletedColumn
addTaskItem placementResults taskItem ((CompletedColumn c tth tl) as completedColumn) =
    let
        filteredPlacements : List PlacementResult
        filteredPlacements =
            placementResults
                |> List.filter (\r -> r /= PlacementResult.DoesNotBelong)

        shouldBeAdded : Bool
        shouldBeAdded =
            filteredPlacements
                |> List.all (\r -> r == PlacementResult.CompletedInThisColumn)
                |> (&&) (not <| List.isEmpty filteredPlacements)
    in
    if shouldBeAdded then
        CompletedColumn c tth (TaskList.add taskItem tl)

    else
        completedColumn


setTagsToHide : List String -> CompletedColumn -> CompletedColumn
setTagsToHide tags completedColumn =
    completedColumn
