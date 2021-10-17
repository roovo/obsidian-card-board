module Panels exposing
    ( Panels
    , cards
    , init
    , panels
    , tabTitles
    )

import Card exposing (Card)
import CardBoard
import Panel exposing (Panel)
import SafeZipper exposing (SafeZipper)
import TaskList exposing (TaskList)
import Time
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Panels
    = Panels (SafeZipper CardBoard.Config) TaskList



-- CONSTRUCTION


init : SafeZipper CardBoard.Config -> TaskList -> Panels
init configs taskList =
    Panels configs taskList



-- INFO


panels : Panels -> SafeZipper Panel
panels (Panels configs taskList) =
    SafeZipper.indexedMapSelectedAndRest (panel taskList) (panel taskList) configs


tabTitles : Panels -> SafeZipper String
tabTitles (Panels configs _) =
    SafeZipper.indexedMapSelectedAndRest tabTitle tabTitle configs


cards : TimeWithZone -> Panels -> List Card
cards timeWithZone ps =
    ps
        |> panels
        |> SafeZipper.toList
        |> List.indexedMap (Panel.columns timeWithZone)
        |> List.concat
        |> List.map Tuple.second
        |> List.concat



-- PRIVATE


tabTitle : Int -> CardBoard.Config -> String
tabTitle _ config =
    case config of
        CardBoard.DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        CardBoard.TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title


panel : TaskList -> Int -> CardBoard.Config -> Panel
panel taskList _ config =
    Panel.init config taskList
