module SettingsState exposing
    ( SettingsState(..)
    , addBoardRequested
    , boardConfigs
    , cancelCurrentState
    , confirmAddBoard
    , confirmDeleteBoard
    , deleteBoardRequested
    , editBoardAt
    , editGlobalSettings
    , globalSettings
    , init
    , updateBoardBeingAdded
    , updateBoardBeingEdited
    , updateGlobalSettings
    )

import BoardConfig exposing (BoardConfig)
import CardBoardSettings
import GlobalSettings exposing (GlobalSettings)
import SafeZipper exposing (SafeZipper)



-- TYPES


type SettingsState
    = AddingBoard BoardConfig (SafeZipper BoardConfig) GlobalSettings
    | ClosingPlugin (SafeZipper BoardConfig) GlobalSettings
    | ClosingSettings (SafeZipper BoardConfig) GlobalSettings
    | DeletingBoard (SafeZipper BoardConfig) GlobalSettings
    | EditingBoard (SafeZipper BoardConfig) GlobalSettings
    | EditingGlobalSettings (SafeZipper BoardConfig) GlobalSettings



-- CREATE


init : SafeZipper BoardConfig -> GlobalSettings -> SettingsState
init cs gs =
    if SafeZipper.length cs == 0 then
        AddingBoard BoardConfig.default cs gs

    else
        EditingBoard cs gs



-- UTILITIES


boardConfigs : SettingsState -> SafeZipper BoardConfig
boardConfigs settingsState =
    case settingsState of
        AddingBoard _ cs _ ->
            cs

        ClosingPlugin cs _ ->
            cs

        ClosingSettings cs _ ->
            cs

        DeletingBoard cs _ ->
            cs

        EditingBoard cs _ ->
            cs

        EditingGlobalSettings cs _ ->
            cs


globalSettings : SettingsState -> GlobalSettings
globalSettings settingsState =
    case settingsState of
        AddingBoard _ _ gs ->
            gs

        ClosingPlugin _ gs ->
            gs

        ClosingSettings _ gs ->
            gs

        DeletingBoard _ gs ->
            gs

        EditingBoard _ gs ->
            gs

        EditingGlobalSettings _ gs ->
            gs



-- TRANSFORM


addBoardRequested : SettingsState -> SettingsState
addBoardRequested settingsState =
    case settingsState of
        AddingBoard _ _ _ ->
            settingsState

        ClosingPlugin cs gs ->
            AddingBoard BoardConfig.default cs gs

        ClosingSettings cs gs ->
            AddingBoard BoardConfig.default cs gs

        DeletingBoard cs gs ->
            AddingBoard BoardConfig.default cs gs

        EditingBoard cs gs ->
            AddingBoard BoardConfig.default cs gs

        EditingGlobalSettings cs gs ->
            AddingBoard BoardConfig.default cs gs


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ cs gs ->
            if SafeZipper.length cs == 0 then
                ClosingPlugin cs gs

            else
                init cs gs

        ClosingPlugin cs gs ->
            ClosingPlugin cs gs

        ClosingSettings cs gs ->
            ClosingSettings cs gs

        DeletingBoard cs gs ->
            EditingBoard cs gs

        EditingBoard cs gs ->
            ClosingSettings cs gs

        EditingGlobalSettings cs gs ->
            ClosingSettings cs gs


confirmAddBoard : SettingsState -> SettingsState
confirmAddBoard settingsState =
    case settingsState of
        AddingBoard c cs gs ->
            EditingBoard (SafeZipper.last <| SafeZipper.add c cs) gs

        _ ->
            settingsState


confirmDeleteBoard : SettingsState -> SettingsState
confirmDeleteBoard settingsState =
    case settingsState of
        DeletingBoard cs gs ->
            init (SafeZipper.deleteCurrent cs) gs

        _ ->
            settingsState


deleteBoardRequested : SettingsState -> SettingsState
deleteBoardRequested settingsState =
    case settingsState of
        AddingBoard c cs gs ->
            DeletingBoard cs gs

        ClosingPlugin cs gs ->
            DeletingBoard cs gs

        ClosingSettings cs gs ->
            DeletingBoard cs gs

        DeletingBoard cs gs ->
            settingsState

        EditingBoard cs gs ->
            DeletingBoard cs gs

        EditingGlobalSettings cs gs ->
            DeletingBoard cs gs


editBoardAt : Int -> SettingsState -> SettingsState
editBoardAt index settingsState =
    case settingsState of
        AddingBoard _ cs gs ->
            EditingBoard (SafeZipper.atIndex index cs) gs

        ClosingPlugin cs gs ->
            EditingBoard (SafeZipper.atIndex index cs) gs

        ClosingSettings cs gs ->
            EditingBoard (SafeZipper.atIndex index cs) gs

        DeletingBoard cs gs ->
            EditingBoard (SafeZipper.atIndex index cs) gs

        EditingBoard cs gs ->
            EditingBoard (SafeZipper.atIndex index cs) gs

        EditingGlobalSettings cs gs ->
            EditingBoard (SafeZipper.atIndex index cs) gs


editGlobalSettings : SettingsState -> SettingsState
editGlobalSettings settingsState =
    case settingsState of
        AddingBoard _ cs gs ->
            EditingGlobalSettings cs gs

        ClosingPlugin cs gs ->
            EditingGlobalSettings cs gs

        ClosingSettings cs gs ->
            EditingGlobalSettings cs gs

        DeletingBoard cs gs ->
            EditingGlobalSettings cs gs

        EditingBoard cs gs ->
            EditingGlobalSettings cs gs

        EditingGlobalSettings _ _ ->
            settingsState


updateBoardBeingAdded : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
updateBoardBeingAdded fn settingsState =
    case settingsState of
        AddingBoard c cs gs ->
            AddingBoard (fn c) cs gs

        _ ->
            settingsState


updateBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
updateBoardBeingEdited fn settingsState =
    case settingsState of
        EditingBoard cs gs ->
            EditingBoard (SafeZipper.mapCurrent fn cs) gs

        _ ->
            settingsState


updateGlobalSettings : (GlobalSettings -> GlobalSettings) -> SettingsState -> SettingsState
updateGlobalSettings fn settingsState =
    case settingsState of
        EditingGlobalSettings cs gs ->
            EditingGlobalSettings cs (fn gs)

        _ ->
            settingsState
