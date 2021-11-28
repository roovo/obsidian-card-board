module SettingsState exposing
    ( Action(..)
    , SettingsState(..)
    , addState
    , cancelCurrentState
    , confirmAdd
    , confirmDelete
    , deleteState
    , init
    , mapBoardBeingAdded
    )

import BoardConfig exposing (BoardConfig)
import InteropPorts
import SafeZipper exposing (SafeZipper)



-- TYPES


type SettingsState
    = AddingBoard BoardConfig
    | DeletingBoard
    | EditingBoard


type Action
    = AddBoard BoardConfig SettingsState
    | AddCancelled SettingsState
    | DeleteCurrent
    | Exit
    | NoAction
    | SetToState SettingsState



-- CONSTRUCTION


init : SafeZipper BoardConfig -> SettingsState
init boardConfigs =
    if SafeZipper.length boardConfigs == 0 then
        AddingBoard BoardConfig.default

    else
        EditingBoard


addState : SettingsState
addState =
    AddingBoard BoardConfig.default


deleteState : SettingsState
deleteState =
    DeletingBoard



-- MAPPING


mapBoardBeingAdded : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingAdded fn settingsState =
    case settingsState of
        AddingBoard c ->
            AddingBoard (fn c)

        _ ->
            settingsState



-- ACTIONS


cancelCurrentState : SettingsState -> Action
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ ->
            AddCancelled EditingBoard

        DeletingBoard ->
            SetToState EditingBoard

        EditingBoard ->
            Exit


confirmAdd : SettingsState -> Action
confirmAdd settingsState =
    case settingsState of
        AddingBoard c ->
            AddBoard c EditingBoard

        EditingBoard ->
            NoAction

        DeletingBoard ->
            NoAction


confirmDelete : SettingsState -> Action
confirmDelete settingsState =
    case settingsState of
        AddingBoard c ->
            NoAction

        EditingBoard ->
            NoAction

        DeletingBoard ->
            DeleteCurrent
