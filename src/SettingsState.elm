module SettingsState exposing
    ( SettingsState(..)
    , addBoardRequested
    , boardConfigs
    , cancelCurrentState
    , confirmAddBoard
    , confirmDeleteBoard
    , deleteBoardRequested
    , init
    , switchBoardBeingEdited
    , updateBoardBeingAdded
    , updateBoardBeingEdited
    )

import BoardConfig exposing (BoardConfig)
import SafeZipper exposing (SafeZipper)



-- TYPES


type SettingsState
    = AddingBoard BoardConfig (SafeZipper BoardConfig)
    | ClosingPlugin (SafeZipper BoardConfig)
    | ClosingSettings (SafeZipper BoardConfig)
    | DeletingBoard (SafeZipper BoardConfig)
    | EditingBoard (SafeZipper BoardConfig)



-- CREATE


init : SafeZipper BoardConfig -> SettingsState
init boardConfigs_ =
    if SafeZipper.length boardConfigs_ == 0 then
        AddingBoard BoardConfig.default boardConfigs_

    else
        EditingBoard boardConfigs_



-- UTILITIES


boardConfigs : SettingsState -> SafeZipper BoardConfig
boardConfigs settingsState =
    case settingsState of
        AddingBoard _ cs ->
            cs

        ClosingPlugin cs ->
            cs

        ClosingSettings cs ->
            cs

        DeletingBoard cs ->
            cs

        EditingBoard cs ->
            cs



-- TRANSFORM


addBoardRequested : SettingsState -> SettingsState
addBoardRequested settingsState =
    case settingsState of
        AddingBoard c cs ->
            settingsState

        ClosingPlugin cs ->
            AddingBoard BoardConfig.default cs

        ClosingSettings cs ->
            AddingBoard BoardConfig.default cs

        DeletingBoard cs ->
            AddingBoard BoardConfig.default cs

        EditingBoard cs ->
            AddingBoard BoardConfig.default cs


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ cs ->
            if SafeZipper.length cs == 0 then
                ClosingPlugin cs

            else
                init cs

        ClosingPlugin cs ->
            ClosingPlugin cs

        ClosingSettings cs ->
            ClosingSettings cs

        DeletingBoard cs ->
            EditingBoard cs

        EditingBoard cs ->
            ClosingSettings cs


confirmAddBoard : SettingsState -> SettingsState
confirmAddBoard settingsState =
    case settingsState of
        AddingBoard c cs ->
            cs
                |> SafeZipper.add c
                |> SafeZipper.last
                |> EditingBoard

        _ ->
            settingsState


confirmDeleteBoard : SettingsState -> SettingsState
confirmDeleteBoard settingsState =
    case settingsState of
        DeletingBoard cs ->
            SafeZipper.deleteCurrent cs
                |> init

        _ ->
            settingsState


deleteBoardRequested : SettingsState -> SettingsState
deleteBoardRequested settingsState =
    case settingsState of
        AddingBoard c cs ->
            DeletingBoard cs

        ClosingPlugin cs ->
            DeletingBoard cs

        ClosingSettings cs ->
            DeletingBoard cs

        DeletingBoard cs ->
            settingsState

        EditingBoard cs ->
            DeletingBoard cs


switchBoardBeingEdited : Int -> SettingsState -> SettingsState
switchBoardBeingEdited index settingsState =
    case settingsState of
        AddingBoard c cs ->
            EditingBoard <| SafeZipper.atIndex index cs

        ClosingPlugin cs ->
            EditingBoard <| SafeZipper.atIndex index cs

        ClosingSettings cs ->
            EditingBoard <| SafeZipper.atIndex index cs

        DeletingBoard cs ->
            EditingBoard <| SafeZipper.atIndex index cs

        EditingBoard cs ->
            EditingBoard <| SafeZipper.atIndex index cs


updateBoardBeingAdded : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
updateBoardBeingAdded fn settingsState =
    case settingsState of
        AddingBoard c cs ->
            AddingBoard (fn c) cs

        _ ->
            settingsState


updateBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
updateBoardBeingEdited fn settingsState =
    case settingsState of
        EditingBoard cs ->
            EditingBoard <| SafeZipper.mapCurrent fn cs

        _ ->
            settingsState
