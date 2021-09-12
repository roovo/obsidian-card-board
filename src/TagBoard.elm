module TagBoard exposing
    ( Config
    , TagBoard
    , columns
    , fill
    )

import Date exposing (Date)
import List.Extra as LE
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type TagBoard
    = TagBoard Config TaskList


type alias Config =
    { columns : List String
    , includeOthers : Bool
    , includeCompleted : Bool
    }


fill : Config -> TaskList -> TagBoard
fill config taskList =
    TagBoard config taskList



-- COLUMNS


columns : TagBoard -> List ( String, List TaskItem )
columns (TagBoard config taskList) =
    config.columns
        |> LE.unique
        |> List.foldl (fillColumn taskList) []
        |> prependOthers config taskList
        |> appendCompleted config taskList



-- PRIVATE


appendCompleted : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
appendCompleted config taskList columnList =
    let
        completedTasks =
            taskList
                |> TaskList.filter isCompleteWithTags
                |> TaskList.placeInColumn "Done"
                |> TaskList.topLevelTasks

        isCompleteWithTags : TaskItem -> Bool
        isCompleteWithTags item =
            TaskItem.isCompleted item && TaskItem.hasOneOfTheTags uniqueColumns item

        uniqueColumns =
            config.columns
                |> LE.unique
    in
    if config.includeCompleted then
        List.append columnList [ ( "Done", completedTasks ) ]

    else
        columnList


prependOthers : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependOthers config taskList columnList =
    let
        otherTasks =
            taskList
                |> TaskList.filter isIncompleteWithoutTags
                |> TaskList.placeInColumn "Others"
                |> TaskList.topLevelTasks

        isIncompleteWithoutTags : TaskItem -> Bool
        isIncompleteWithoutTags item =
            not (TaskItem.isCompleted item) && not (TaskItem.hasOneOfTheTags uniqueColumns item)

        uniqueColumns =
            config.columns
                |> LE.unique
    in
    if config.includeOthers then
        ( "Others", otherTasks ) :: columnList

    else
        columnList


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
