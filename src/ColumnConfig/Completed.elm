module ColumnConfig.Completed exposing
    ( CompletedColumn
    , addTaskItem
    , asColumn
    , init
    )

import Column exposing (Column, PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type CompletedColumn
    = CompletedColumn Config TaskList


type alias Config =
    { name : String
    , limit : Int
    }



-- CONSTRUCTION


init : String -> Int -> CompletedColumn
init name_ limit_ =
    CompletedColumn { name = name_, limit = limit_ } TaskList.empty



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
