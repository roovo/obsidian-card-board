module TagBoard exposing
    ( Config
    , columns
    )

import Date exposing (Date)
import List.Extra as LE
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type alias Config =
    { columns : List ColumnConfig
    , includeOthers : Bool
    , includeCompleted : Bool
    , title : String
    }


type alias ColumnConfig =
    { tag : String
    , displayTitle : String
    }



-- COLUMNS


columns : Config -> Int -> TaskList -> List ( String, List TaskItem )
columns config panelIndex taskList =
    config.columns
        |> LE.uniqueBy .tag
        |> List.foldl (fillColumn panelIndex taskList) []
        |> prependOthers config panelIndex taskList
        |> appendCompleted config panelIndex taskList



-- PRIVATE


appendCompleted : Config -> Int -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
appendCompleted config panelIndex taskList columnList =
    let
        completedTasks =
            taskList
                |> TaskList.filter isCompleteWithTags
                |> TaskList.topLevelTasks
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.reverse
                |> List.sortBy TaskItem.completedPosix
                |> List.reverse

        isCompleteWithTags : TaskItem -> Bool
        isCompleteWithTags item =
            TaskItem.isCompleted item && TaskItem.hasOneOfTheTags uniqueColumnTags item

        uniqueColumnTags =
            config.columns
                |> LE.uniqueBy .tag
                |> List.map .tag
    in
    if config.includeCompleted then
        List.append columnList [ ( "Completed", completedTasks ) ]

    else
        columnList


prependOthers : Config -> Int -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependOthers config panelIndex taskList columnList =
    let
        cards =
            taskList
                |> TaskList.filter isIncompleteWithoutTags
                |> TaskList.topLevelTasks
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.sortBy TaskItem.dueRataDie

        isIncompleteWithoutTags : TaskItem -> Bool
        isIncompleteWithoutTags item =
            not (TaskItem.isCompleted item) && not (TaskItem.hasOneOfTheTags uniqueColumnTags item)

        uniqueColumnTags =
            config.columns
                |> LE.uniqueBy .tag
                |> List.map .tag
    in
    if config.includeOthers then
        ( "Others", cards ) :: columnList

    else
        columnList


fillColumn : Int -> TaskList -> ColumnConfig -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
fillColumn panelIndex taskList columnConfig acc =
    let
        isIncompleteWithTag : String -> TaskItem -> Bool
        isIncompleteWithTag tag item =
            not (TaskItem.isCompleted item) && TaskItem.hasTag tag item
    in
    TaskList.filter (isIncompleteWithTag columnConfig.tag) taskList
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Tuple.pair columnConfig.displayTitle
        |> List.singleton
        |> List.append acc
