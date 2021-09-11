module TagBoard exposing
    ( TagBoard
    , columns
    , fill
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type TagBoard
    = TagBoard Config TaskList


type alias Config =
    { columns : List String
    }


fill : Config -> TaskList -> TagBoard
fill config taskList =
    TagBoard config taskList



-- COLUMNS


columns : TagBoard -> List ( String, List TaskItem )
columns (TagBoard config taskList) =
    List.foldl (fillColumn taskList) [] config.columns



-- PRIVATE


fillColumn : TaskList -> String -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
fillColumn taskList column acc =
    let
        isIncompleteWithTag : String -> TaskItem -> Bool
        isIncompleteWithTag tag item =
            not (TaskItem.isCompleted item) && TaskItem.hasTag tag item
    in
    TaskList.filter (isIncompleteWithTag column) taskList
        |> TaskList.placeInColumn column
        |> TaskList.topLevelTasks
        |> Tuple.pair column
        |> List.singleton
        |> List.append acc
