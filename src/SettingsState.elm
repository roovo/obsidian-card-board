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
    , init
    , mapBoardBeingAdded
    , mapBoardBeingEdited
    , mapGlobalSettings
    , settings
    )

import BoardConfig exposing (BoardConfig)
import GlobalSettings exposing (GlobalSettings)
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)



-- TYPES


type SettingsState
    = AddingBoard BoardConfig Settings
    | ClosingPlugin Settings
    | ClosingSettings Settings
    | DeletingBoard Settings
    | EditingBoard Settings
    | EditingGlobalSettings Settings



-- CREATE


init : Settings -> SettingsState
init settings_ =
    if Settings.hasAnyBordsConfigured settings_ then
        EditingBoard settings_

    else
        AddingBoard BoardConfig.default settings_



-- UTILITIES


boardConfigs : SettingsState -> SafeZipper BoardConfig
boardConfigs settingsState =
    Settings.boardConfigs <| settings settingsState


settings : SettingsState -> Settings
settings settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            settings_

        ClosingPlugin settings_ ->
            settings_

        ClosingSettings settings_ ->
            settings_

        DeletingBoard settings_ ->
            settings_

        EditingBoard settings_ ->
            settings_

        EditingGlobalSettings settings_ ->
            settings_



-- TRANSFORM


addBoardRequested : SettingsState -> SettingsState
addBoardRequested settingsState =
    case settingsState of
        AddingBoard _ _ ->
            settingsState

        ClosingPlugin settings_ ->
            AddingBoard BoardConfig.default settings_

        ClosingSettings settings_ ->
            AddingBoard BoardConfig.default settings_

        DeletingBoard settings_ ->
            AddingBoard BoardConfig.default settings_

        EditingBoard settings_ ->
            AddingBoard BoardConfig.default settings_

        EditingGlobalSettings settings_ ->
            AddingBoard BoardConfig.default settings_


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            if Settings.hasAnyBordsConfigured settings_ then
                init settings_

            else
                ClosingPlugin settings_

        ClosingPlugin settings_ ->
            ClosingPlugin settings_

        ClosingSettings settings_ ->
            ClosingSettings settings_

        DeletingBoard settings_ ->
            EditingBoard settings_

        EditingBoard settings_ ->
            ClosingSettings settings_

        EditingGlobalSettings settings_ ->
            ClosingSettings settings_


confirmAddBoard : SettingsState -> SettingsState
confirmAddBoard settingsState =
    case settingsState of
        AddingBoard c settings_ ->
            if String.isEmpty <| BoardConfig.title c then
                EditingBoard <| Settings.addBoard (BoardConfig.updateTitle "Untitled" c) settings_

            else
                EditingBoard <| Settings.addBoard c settings_

        _ ->
            settingsState


confirmDeleteBoard : SettingsState -> SettingsState
confirmDeleteBoard settingsState =
    case settingsState of
        DeletingBoard settings_ ->
            init (Settings.deleteCurrentBoard settings_)

        _ ->
            settingsState


deleteBoardRequested : SettingsState -> SettingsState
deleteBoardRequested settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            DeletingBoard settings_

        ClosingPlugin settings_ ->
            DeletingBoard settings_

        ClosingSettings settings_ ->
            DeletingBoard settings_

        DeletingBoard _ ->
            settingsState

        EditingBoard settings_ ->
            DeletingBoard settings_

        EditingGlobalSettings settings_ ->
            DeletingBoard settings_


editBoardAt : Int -> SettingsState -> SettingsState
editBoardAt index settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            EditingBoard (Settings.switchToBoard index settings_)

        ClosingPlugin settings_ ->
            EditingBoard (Settings.switchToBoard index settings_)

        ClosingSettings settings_ ->
            EditingBoard (Settings.switchToBoard index settings_)

        DeletingBoard settings_ ->
            EditingBoard (Settings.switchToBoard index settings_)

        EditingBoard settings_ ->
            EditingBoard (Settings.switchToBoard index settings_)

        EditingGlobalSettings settings_ ->
            EditingBoard (Settings.switchToBoard index settings_)


editGlobalSettings : SettingsState -> SettingsState
editGlobalSettings settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            EditingGlobalSettings settings_

        ClosingPlugin settings_ ->
            EditingGlobalSettings settings_

        ClosingSettings settings_ ->
            EditingGlobalSettings settings_

        DeletingBoard settings_ ->
            EditingGlobalSettings settings_

        EditingBoard settings_ ->
            EditingGlobalSettings settings_

        EditingGlobalSettings _ ->
            settingsState


mapBoardBeingAdded : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingAdded fn settingsState =
    case settingsState of
        AddingBoard c settings_ ->
            AddingBoard (fn c) settings_

        _ ->
            settingsState


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingEdited fn settingsState =
    case settingsState of
        EditingBoard settings_ ->
            EditingBoard <| Settings.updateCurrentBoard fn settings_

        _ ->
            settingsState


mapGlobalSettings : (GlobalSettings -> GlobalSettings) -> SettingsState -> SettingsState
mapGlobalSettings fn settingsState =
    case settingsState of
        EditingGlobalSettings settings_ ->
            EditingGlobalSettings (Settings.mapGlobalSettings fn settings_)

        _ ->
            settingsState
