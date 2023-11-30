module ColumnConfig.Completed exposing
    ( CompletedColumn
    , addTaskItem
    , index
    , init
    , isCollapsed
    , name
    , taskList
    )

import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type CompletedColumn
    = CompletedColumn Config TaskList


type alias Config =
    { collapsed : Bool
    , index : Int
    , limit : Int
    , name : String
    }



-- CONSTRUCTION


init : String -> Int -> Int -> CompletedColumn
init name_ index_ limit_ =
    CompletedColumn { collapsed = False, index = index_, limit = limit_, name = name_ } TaskList.empty



-- INFO


index : CompletedColumn -> Int
index (CompletedColumn c _) =
    c.index


isCollapsed : CompletedColumn -> Bool
isCollapsed (CompletedColumn c _) =
    c.collapsed


name : CompletedColumn -> String
name (CompletedColumn c _) =
    c.name


taskList : CompletedColumn -> TaskList
taskList (CompletedColumn _ tl) =
    tl



-- MANIPULATION


addTaskItem : List PlacementResult -> TaskItem -> CompletedColumn -> CompletedColumn
addTaskItem placementResults taskItem ((CompletedColumn c tl) as completedColumn) =
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
        CompletedColumn c (TaskList.add taskItem tl)

    else
        completedColumn
