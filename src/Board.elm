module Board exposing
    ( Board
    , columns
    , init
    )

import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import DateBoard
import Filter exposing (Filter)
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Board
    = Board BoardConfig TaskList



-- CONSTRUCTION


init : BoardConfig -> TaskList -> Board
init config taskList =
    Board config taskList



-- INFO


columns : TimeWithZone -> Int -> Board -> List ( String, List Card )
columns timeWithZone boardIndex (Board config taskList) =
    case config of
        BoardConfig.DateBoardConfig dateBoardConfig ->
            taskList
                |> filterTaskList config
                |> DateBoard.columns timeWithZone dateBoardConfig
                |> placeCardsInColumns boardIndex

        BoardConfig.TagBoardConfig tagBoardConfig ->
            TagBoard.columns tagBoardConfig taskList
                |> placeCardsInColumns boardIndex



-- PRIVATE


filterTaskList : BoardConfig -> TaskList -> TaskList
filterTaskList config taskList =
    let
        filters =
            BoardConfig.filters config
    in
    taskList
        |> filterByFilesystem filters
        |> filterByTag filters


filterByFilesystem : List Filter -> TaskList -> TaskList
filterByFilesystem filters taskList =
    List.filter (\f -> Filter.filterType f == "Files" || Filter.filterType f == "Paths") filters
        |> applyFilters taskList


filterByTag : List Filter -> TaskList -> TaskList
filterByTag filters taskList =
    List.filter (\f -> Filter.filterType f == "Tags") filters
        |> applyFilters taskList


applyFilters : TaskList -> List Filter -> TaskList
applyFilters taskList filters =
    if List.isEmpty filters then
        taskList

    else
        TaskList.filter (\x -> List.any (Filter.isAllowed x) filters) taskList


placeCardsInColumns : Int -> List ( String, List TaskItem ) -> List ( String, List Card )
placeCardsInColumns boardIndex columnList =
    let
        cardIdPrefix : Int -> String
        cardIdPrefix columnIndex =
            String.fromInt boardIndex ++ ":" ++ String.fromInt columnIndex

        placeCardsInColumn : Int -> ( String, List TaskItem ) -> ( String, List Card )
        placeCardsInColumn columnIndex ( columnTitle, taskItems ) =
            taskItems
                |> List.map (Card.fromTaskItem <| cardIdPrefix columnIndex)
                |> Tuple.pair columnTitle
    in
    columnList
        |> List.indexedMap placeCardsInColumn
