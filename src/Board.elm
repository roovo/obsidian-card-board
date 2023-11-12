module Board exposing
    ( Board
    , columns
    , init
    )

import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import CollapsedColumns exposing (CollapsedColumns)
import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import DateBoardColumns exposing (DateBoardColumns)
import Filter exposing (Filter, Polarity, Scope)
import TagBoardColumns exposing (TagBoardColumns)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Board
    = Board String ColumnNames BoardConfig TaskList



-- CONSTRUCTION


init : String -> ColumnNames -> BoardConfig -> TaskList -> Board
init uniqueId columnNames config taskList =
    Board uniqueId columnNames config taskList



-- INFO


columns : Bool -> TimeWithZone -> Board -> List (Column Card)
columns ignoreFileNameDates timeWithZone (Board uniqueId columnNames config taskList) =
    case config of
        BoardConfig.DateBoardConfig dateBoardConfig ->
            let
                emptyDateBoardColumns : DateBoardColumns
                emptyDateBoardColumns =
                    DateBoardColumns.init timeWithZone columnNames dateBoardConfig
            in
            taskList
                |> filterTaskList config
                |> configureDueDates ignoreFileNameDates
                |> TaskList.foldl DateBoardColumns.addTaskItem emptyDateBoardColumns
                |> DateBoardColumns.columns
                |> convertToCards uniqueId (BoardConfig.title config)
                |> collapseColumns config

        BoardConfig.TagBoardConfig tagBoardConfig ->
            let
                emptyTagBoardColumns : TagBoardColumns
                emptyTagBoardColumns =
                    TagBoardColumns.init columnNames tagBoardConfig
            in
            taskList
                |> filterTaskList config
                |> configureDueDates ignoreFileNameDates
                |> TaskList.foldl TagBoardColumns.addTaskItem emptyTagBoardColumns
                |> TagBoardColumns.columns
                |> convertToCards uniqueId (BoardConfig.title config)
                |> collapseColumns config



-- PRIVATE


collapseColumns : BoardConfig -> List (Column Card) -> List (Column Card)
collapseColumns config cols =
    let
        collapsedColumns : CollapsedColumns
        collapsedColumns =
            BoardConfig.collapsedColumns config

        performCollapse : Int -> Column Card -> Column Card
        performCollapse index col =
            if CollapsedColumns.columnIsCollapsed index collapsedColumns then
                Column.collapseState True col

            else
                Column.collapseState False col
    in
    List.indexedMap performCollapse cols


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


convertToCards : String -> String -> List (Column TaskItem) -> List (Column Card)
convertToCards uniqueId boardTitle columnList =
    let
        cardIdPrefix : Int -> String
        cardIdPrefix columnIndex =
            uniqueId ++ ":" ++ boardTitle ++ ":" ++ String.fromInt columnIndex

        placeCardsInColumn : Int -> Column TaskItem -> Column Card
        placeCardsInColumn columnIndex column =
            Column.items column
                |> List.map (Card.fromTaskItem <| cardIdPrefix columnIndex)
                |> Column.init True (Column.name column)
    in
    columnList
        |> List.indexedMap placeCardsInColumn
