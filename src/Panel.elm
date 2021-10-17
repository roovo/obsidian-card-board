module Panel exposing
    ( Panel
    , columns
    , init
    )

import Card exposing (Card)
import CardBoard
import DateBoard
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type Panel
    = Panel CardBoard.Config TaskList



-- CONSTRUCTION


init : CardBoard.Config -> TaskList -> Panel
init config taskList =
    Panel config taskList



-- INFO


columns : Time.Posix -> Time.Zone -> Int -> Panel -> List ( String, List Card )
columns now zone panelIndex (Panel config taskList) =
    case config of
        CardBoard.DateBoardConfig dateBoardConfig ->
            DateBoard.columns now zone dateBoardConfig taskList
                |> placeCardsInColumns panelIndex

        CardBoard.TagBoardConfig tagBoardConfig ->
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



-- Card (String.fromInt panelIndex ++ ":" ++ columnId ++ ":") item
