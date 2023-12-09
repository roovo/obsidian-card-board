module Board exposing
    ( Board
    , columns
    , id
    , init
    )

import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import Column exposing (Column)
import Columns
import Date exposing (Date)
import Filter exposing (Filter, Polarity, Scope)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type Board
    = Board String BoardConfig TaskList



-- CONSTRUCTION


init : String -> BoardConfig -> TaskList -> Board
init uniqueId config taskList =
    Board uniqueId config taskList



-- INFO


columns : Bool -> Date -> Board -> List Column
columns ignoreFileNameDates today ((Board _ config taskList) as board) =
    taskList
        |> filterTaskList config
        |> configureDueDates ignoreFileNameDates
        |> Columns.addTaskList today (tagsToHide config) (BoardConfig.columns config)
        |> Columns.toList


id : Board -> String
id (Board uniqueId config _) =
    uniqueId ++ ":" ++ String.replace " " "_" (BoardConfig.title config)



-- PRIVATE


applyFilters : TaskList -> Polarity -> Scope -> List Filter -> TaskList
applyFilters taskList polarity scope filters =
    let
        operator : Bool -> Bool
        operator =
            case polarity of
                Filter.Allow ->
                    identity

                Filter.Deny ->
                    not

        filterMode : (a -> Bool) -> List a -> Bool
        filterMode =
            case polarity of
                Filter.Allow ->
                    List.any

                Filter.Deny ->
                    List.all
    in
    if List.isEmpty filters then
        taskList

    else
        TaskList.filter (\t -> filterMode (operator << Filter.isAllowed scope t) filters) taskList


configureDueDates : Bool -> TaskList -> TaskList
configureDueDates ignoreFileNameDates taskList =
    if ignoreFileNameDates then
        TaskList.map TaskItem.removeFileNameDate taskList

    else
        taskList


filterTaskList : BoardConfig -> TaskList -> TaskList
filterTaskList config taskList =
    let
        filters : List Filter
        filters =
            BoardConfig.filters config

        filterPolarity : Polarity
        filterPolarity =
            BoardConfig.filterPolarity config

        filterScope : Scope
        filterScope =
            BoardConfig.filterScope config
    in
    taskList
        |> filterByFilesystem filterPolarity filterScope filters
        |> filterByTag filterPolarity filterScope filters


filterByFilesystem : Polarity -> Scope -> List Filter -> TaskList -> TaskList
filterByFilesystem polarity scope filters taskList =
    List.filter (\f -> Filter.filterType f == "Files" || Filter.filterType f == "Paths") filters
        |> applyFilters taskList polarity scope


filterByTag : Polarity -> Scope -> List Filter -> TaskList -> TaskList
filterByTag polarity scope filters taskList =
    List.filter (\f -> Filter.filterType f == "Tags") filters
        |> applyFilters taskList polarity scope


tagsToHide : BoardConfig -> List String
tagsToHide boardConfig =
    let
        columnTagsToHide : List String
        columnTagsToHide =
            if BoardConfig.showColumnTags boardConfig then
                []

            else
                Columns.namedTagColumnTags <| BoardConfig.columns boardConfig

        filterTagsToHide : List String
        filterTagsToHide =
            if BoardConfig.showFilteredTags boardConfig then
                []

            else
                boardConfig
                    |> BoardConfig.filters
                    |> List.filter (\f -> Filter.filterType f == "Tags")
                    |> List.map Filter.value
    in
    columnTagsToHide ++ filterTagsToHide
