module Panels exposing
    ( Panels
    , init
    , panels
    , tabTitles
    )

import CardBoard
import Panel exposing (Panel)
import SafeZipper exposing (SafeZipper)
import TaskList exposing (TaskList)



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



-- PRIVATE


tabTitle : Int -> CardBoard.Config -> String
tabTitle _ config =
    CardBoard.title config


panel : TaskList -> Int -> CardBoard.Config -> Panel
panel taskList _ config =
    Panel.init config taskList
