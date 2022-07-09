module Boards exposing
    ( Boards
    , boardZipper
    , cards
    , currentIndex
    , init
    , length
    , titles
    )

import Board exposing (Board)
import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import Column
import SafeZipper exposing (SafeZipper)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Boards
    = Boards (SafeZipper BoardConfig) TaskList



-- CONSTRUCTION


init : SafeZipper BoardConfig -> TaskList -> Boards
init configs taskList =
    Boards configs taskList



-- INFO


boardZipper : Boards -> SafeZipper Board
boardZipper (Boards configs taskList) =
    SafeZipper.indexedMapSelectedAndRest (board taskList) (board taskList) configs


titles : Boards -> SafeZipper String
titles (Boards configs _) =
    SafeZipper.indexedMapSelectedAndRest tabTitle tabTitle configs


cards : TimeWithZone -> Boards -> List Card
cards timeWithZone boards_ =
    boards_
        |> boardZipper
        |> SafeZipper.toList
        |> List.indexedMap (Board.columns timeWithZone)
        |> List.concat
        |> List.map Column.items
        |> List.concat


currentIndex : Boards -> Maybe Int
currentIndex (Boards config _) =
    SafeZipper.selectedIndex config


length : Boards -> Int
length (Boards config _) =
    SafeZipper.length config



-- PRIVATE


tabTitle : Int -> BoardConfig -> String
tabTitle _ config =
    case config of
        BoardConfig.DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        BoardConfig.TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title


board : TaskList -> Int -> BoardConfig -> Board
board taskList _ config =
    Board.init config taskList
