module Boards exposing
    ( Boards
    , boardZipper
    , cards
    , currentIndex
    , init
    , names
    )

import Board exposing (Board)
import BoardConfig exposing (BoardConfig)
import Card exposing (Card)
import Column exposing (Column)
import Date exposing (Date)
import SafeZipper exposing (SafeZipper)
import TaskList exposing (TaskList)



-- TYPES


type Boards
    = Boards String (SafeZipper BoardConfig) TaskList



-- CONSTRUCTION


init : String -> SafeZipper BoardConfig -> TaskList -> Boards
init uniqueId configs taskList =
    Boards uniqueId configs taskList



-- INFO


boardZipper : Boards -> SafeZipper Board
boardZipper (Boards uniqueId configs taskList) =
    SafeZipper.map (board uniqueId taskList) configs


cards : Bool -> Date -> Boards -> List Card
cards ignoreFileNameDates today boards_ =
    let
        cardsForBoard : String -> List Column -> List (List Card)
        cardsForBoard boardId columns =
            List.map (Column.cards boardId) columns
    in
    boards_
        |> boardZipper
        |> SafeZipper.toList
        |> List.map (\b -> ( Board.id b, Board.columns ignoreFileNameDates today b ))
        |> List.map (\( bid, cs ) -> cardsForBoard bid cs)
        |> List.concat
        |> List.concat


currentIndex : Boards -> Maybe Int
currentIndex (Boards _ config _) =
    SafeZipper.selectedIndex config


names : Boards -> SafeZipper String
names (Boards _ configs _) =
    SafeZipper.map tabName configs



-- PRIVATE


tabName : BoardConfig -> String
tabName =
    BoardConfig.name


board : String -> TaskList -> BoardConfig -> Board
board uniqueId taskList config =
    Board.init uniqueId config taskList
