module Panel exposing
    ( Panel
    , columns
    , init
    )

import Card exposing (Card)
import CardBoardConfig
import DateBoard
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Panel
    = Panel CardBoardConfig.Config TaskList



-- CONSTRUCTION


init : CardBoardConfig.Config -> TaskList -> Panel
init config taskList =
    Panel config taskList



-- INFO


columns : TimeWithZone -> Int -> Panel -> List ( String, List Card )
columns timeWithZone panelIndex (Panel config taskList) =
    case config of
        CardBoardConfig.DateBoardConfig dateBoardConfig ->
            DateBoard.columns timeWithZone dateBoardConfig taskList
                |> placeCardsInColumns panelIndex

        CardBoardConfig.TagBoardConfig tagBoardConfig ->
            TagBoard.columns tagBoardConfig taskList
                |> placeCardsInColumns panelIndex



-- PRIVATE


placeCardsInColumns : Int -> List ( String, List TaskItem ) -> List ( String, List Card )
placeCardsInColumns panelIndex columnList =
    let
        cardIdPrefix columnTitle =
            String.fromInt panelIndex ++ ":" ++ columnTitle ++ ":"

        placeCardsInColumn ( columnTitle, taskItems ) =
            taskItems
                |> List.map (Card.fromTaskItem <| cardIdPrefix columnTitle)
                |> Tuple.pair columnTitle
    in
    columnList
        |> List.map placeCardsInColumn
