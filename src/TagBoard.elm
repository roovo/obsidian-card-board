module TagBoard exposing
    ( ColumnConfig
    , Config
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
    , completedCount : Int
    , includeOthers : Bool
    , includeUntagged : Bool
    , title : String
    }


type alias ColumnConfig =
    { tag : String
    , displayTitle : String
    }



-- COLUMNS


columns : Config -> TaskList -> List ( String, List TaskItem )
columns config taskList =
    config.columns
        |> LE.uniqueBy .tag
        |> List.foldl (fillColumn taskList) []
        |> prependOthers config taskList
        |> prependUntagged config taskList
        |> appendCompleted config taskList



-- PRIVATE


appendCompleted : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
appendCompleted config taskList columnList =
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
    if config.completedCount > 0 then
        List.append columnList [ ( "Completed", completedTasks ) ]

    else
        columnList


prependOthers : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependOthers config taskList columnList =
    let
        cards =
            taskList
                |> TaskList.filter isIncompleteWithoutTags
                |> TaskList.topLevelTasks
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.sortBy TaskItem.dueRataDie

        isIncompleteWithoutTags : TaskItem -> Bool
        isIncompleteWithoutTags item =
            not (TaskItem.isCompleted item) && TaskItem.hasTags item && not (TaskItem.hasOneOfTheTags uniqueColumnTags item)

        uniqueColumnTags =
            config.columns
                |> LE.uniqueBy .tag
                |> List.map .tag
    in
    if config.includeOthers then
        ( "Others", cards ) :: columnList

    else
        columnList


prependUntagged : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependUntagged config taskList columnList =
    let
        cards =
            taskList
                |> TaskList.filter isIncompleteWithNoTags
                |> TaskList.topLevelTasks
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.sortBy TaskItem.dueRataDie

        isIncompleteWithNoTags : TaskItem -> Bool
        isIncompleteWithNoTags item =
            not (TaskItem.isCompleted item) && not (TaskItem.hasTags item)

        uniqueColumnTags =
            config.columns
                |> LE.uniqueBy .tag
                |> List.map .tag
    in
    if config.includeUntagged then
        ( "Untagged", cards ) :: columnList

    else
        columnList


fillColumn : TaskList -> ColumnConfig -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
fillColumn taskList columnConfig acc =
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
