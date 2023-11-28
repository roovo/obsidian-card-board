module ColumnConfig.Completed exposing
    ( CompletedColumn
    , addTaskItem
    , asColumn
    , index
    , init
    )

import Column exposing (Column, PlacementResult)
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


asColumn : CompletedColumn -> Column TaskItem
asColumn ((CompletedColumn c tl) as completedColumn) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.reverse
        |> List.sortBy TaskItem.completedPosix
        |> List.reverse
        |> List.take c.limit
        |> Column.init True (name completedColumn) []
        |> Column.collapseState c.collapsed


index : CompletedColumn -> Int
index (CompletedColumn c _) =
    c.index


name : CompletedColumn -> String
name (CompletedColumn c _) =
    c.name



-- MANIPULATION


addTaskItem : List PlacementResult -> TaskItem -> CompletedColumn -> CompletedColumn
addTaskItem placementResults taskItem ((CompletedColumn c tl) as completedColumn) =
    let
        filteredPlacements : List PlacementResult
        filteredPlacements =
            placementResults
                |> List.filter (\r -> r /= Column.DoesNotBelong)

        shouldBeAdded : Bool
        shouldBeAdded =
            filteredPlacements
                |> List.all (\r -> r == Column.CompletedInThisColumn)
                |> (&&) (not <| List.isEmpty filteredPlacements)
    in
    if shouldBeAdded then
        CompletedColumn c (TaskList.add taskItem tl)

    else
        completedColumn
