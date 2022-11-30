module Board exposing
    ( Board
    , columns
    , init
    )

import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import DateBoard
import Filter exposing (Filter, Polarity)
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Board
    = Board ColumnNames BoardConfig TaskList



-- CONSTRUCTION


init : ColumnNames -> BoardConfig -> TaskList -> Board
init columnNames config taskList =
    Board columnNames config taskList



-- INFO


columns : TimeWithZone -> Int -> Board -> List (Column Card)
columns timeWithZone boardIndex (Board columnNames config taskList) =
    case config of
        BoardConfig.DateBoardConfig dateBoardConfig ->
            taskList
                |> filterTaskList config
                |> DateBoard.columns columnNames timeWithZone dateBoardConfig
                |> placeCardsInColumns boardIndex

        BoardConfig.TagBoardConfig tagBoardConfig ->
            taskList
                |> filterTaskList config
                |> TagBoard.columns columnNames tagBoardConfig
                |> placeCardsInColumns boardIndex



-- PRIVATE


filterTaskList : BoardConfig -> TaskList -> TaskList
filterTaskList config taskList =
    let
        filters : List Filter
        filters =
            BoardConfig.filters config

        filterPolarity : Polarity
        filterPolarity =
            BoardConfig.filterPolarity config
    in
    taskList
        |> filterByFilesystem filterPolarity filters
        |> filterByTag filterPolarity filters


filterByFilesystem : Polarity -> List Filter -> TaskList -> TaskList
filterByFilesystem polarity filters taskList =
    List.filter (\f -> Filter.filterType f == "Files" || Filter.filterType f == "Paths") filters
        |> applyFilters taskList polarity


filterByTag : Polarity -> List Filter -> TaskList -> TaskList
filterByTag polarity filters taskList =
    List.filter (\f -> Filter.filterType f == "Tags") filters
        |> applyFilters taskList polarity


applyFilters : TaskList -> Polarity -> List Filter -> TaskList
applyFilters taskList polarity filters =
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
        TaskList.filter (\t -> filterMode (operator << Filter.isAllowed t) filters) taskList


placeCardsInColumns : Int -> List (Column TaskItem) -> List (Column Card)
placeCardsInColumns boardIndex columnList =
    let
        cardIdPrefix : Int -> String
        cardIdPrefix columnIndex =
            String.fromInt boardIndex ++ ":" ++ String.fromInt columnIndex

        placeCardsInColumn : Int -> Column TaskItem -> Column Card
        placeCardsInColumn columnIndex column =
            Column.items column
                |> List.map (Card.fromTaskItem <| cardIdPrefix columnIndex)
                |> Column.init (Column.name column)
    in
    columnList
        |> List.indexedMap placeCardsInColumn
