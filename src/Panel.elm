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


columns : Time.Posix -> Time.Zone -> Int -> Panel -> List ( String, List Card )
columns now zone panelIndex (Panel config taskList) =
    case config of
        CardBoard.DateBoardConfig dateBoardConfig ->
            DateBoard.columns now zone dateBoardConfig panelIndex taskList
                |> cardsInColumns panelIndex

        CardBoard.TagBoardConfig tagBoardConfig ->
            TagBoard.columns tagBoardConfig panelIndex taskList
                |> cardsInColumns panelIndex


cardsInColumns : Int -> List ( String, List TaskItem ) -> List ( String, List Card )
cardsInColumns panelIndex columnList =
    columnList
        |> List.map (\( columnTitle, taskItems ) -> ( columnTitle, List.map (Card.fromTaskItem panelIndex columnTitle) taskItems ))
