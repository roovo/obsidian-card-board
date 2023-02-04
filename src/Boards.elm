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
    = Boards String ColumnNames (SafeZipper BoardConfig) TaskList



-- CONSTRUCTION


init : String -> ColumnNames -> SafeZipper BoardConfig -> TaskList -> Boards
init uniqueId columnNames configs taskList =
    Boards uniqueId columnNames configs taskList



-- INFO


boardZipper : Boards -> SafeZipper Board
boardZipper (Boards uniqueId columnNames configs taskList) =
    SafeZipper.indexedMapSelectedAndRest
        (board uniqueId columnNames taskList)
        (board uniqueId columnNames taskList)
        configs


titles : Boards -> SafeZipper String
titles (Boards _ _ configs _) =
    SafeZipper.indexedMapSelectedAndRest tabTitle tabTitle configs


cards : Bool -> TimeWithZone -> Boards -> List Card
cards ignoreFileNameDates timeWithZone boards_ =
    boards_
        |> boardZipper
        |> SafeZipper.toList
        |> List.indexedMap (Board.columns ignoreFileNameDates timeWithZone)
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


tabTitle : Int -> BoardConfig -> String
tabTitle _ config =
    case config of
        BoardConfig.DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        BoardConfig.TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title


board : String -> ColumnNames -> TaskList -> Int -> BoardConfig -> Board
board uniqueId columnNames taskList _ config =
    Board.init uniqueId columnNames config taskList
