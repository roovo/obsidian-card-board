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
import ColumnNames exposing (ColumnNames)
import SafeZipper exposing (SafeZipper)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Boards
    = Boards ColumnNames (SafeZipper BoardConfig) TaskList



-- CONSTRUCTION


init : ColumnNames -> SafeZipper BoardConfig -> TaskList -> Boards
init columnNames configs taskList =
    Boards columnNames configs taskList



-- INFO


boardZipper : Boards -> SafeZipper Board
boardZipper (Boards columnNames configs taskList) =
    SafeZipper.indexedMapSelectedAndRest (board columnNames taskList) (board columnNames taskList) configs


titles : Boards -> SafeZipper String
titles (Boards _ configs _) =
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
currentIndex (Boards _ config _) =
    SafeZipper.selectedIndex config


length : Boards -> Int
length (Boards _ config _) =
    SafeZipper.length config



-- PRIVATE


tabTitle : Int -> BoardConfig -> String
tabTitle _ config =
    case config of
        BoardConfig.DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        BoardConfig.TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title


board : ColumnNames -> TaskList -> Int -> BoardConfig -> Board
board columnNames taskList _ config =
    Board.init columnNames config taskList
