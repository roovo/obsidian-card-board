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
import Date exposing (Date)
import SafeZipper exposing (SafeZipper)
import TaskList exposing (TaskList)



-- TYPES


type Boards
    = Boards String ColumnNames (SafeZipper BoardConfig) TaskList



-- CONSTRUCTION


init : String -> ColumnNames -> SafeZipper BoardConfig -> TaskList -> Boards
init uniqueId columnNames configs taskList =
    Boards uniqueId columnNames configs taskList



-- INFO


boardZipper : Boards -> SafeZipper Board
boardZipper (Boards uniqueId columnNames configs taskList) =
    SafeZipper.map (board uniqueId columnNames taskList) configs


titles : Boards -> SafeZipper String
titles (Boards _ _ configs _) =
    SafeZipper.map tabTitle configs


cards : Bool -> Date -> Boards -> List Card
cards ignoreFileNameDates today boards_ =
    boards_
        |> boardZipper
        |> SafeZipper.toList
        |> List.map (Board.columns ignoreFileNameDates today)
        |> List.concat
        |> List.map Column.items
        |> List.concat


currentIndex : Boards -> Maybe Int
currentIndex (Boards _ _ config _) =
    SafeZipper.selectedIndex config


length : Boards -> Int
length (Boards _ _ config _) =
    SafeZipper.length config



-- PRIVATE


tabTitle : BoardConfig -> String
tabTitle config =
    case config of
        BoardConfig.DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        BoardConfig.TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title


board : String -> ColumnNames -> TaskList -> BoardConfig -> Board
board uniqueId columnNames taskList config =
    Board.init uniqueId columnNames config taskList
