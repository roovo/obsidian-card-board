module SettingsState exposing
    ( SettingsState(..)
    , addBoardRequested
    , addColumnRequested
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
    , moveBoard
    , settings
    )

import BoardConfig exposing (BoardConfig)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition exposing (BeaconPosition)
import GlobalSettings exposing (GlobalSettings)
import NewBoardConfig exposing (NewBoardConfig)
import NewColumnConfig exposing (NewColumnConfig)
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)



-- TYPES


type SettingsState
    = AddingBoard NewBoardConfig Settings
    | AddingColumn NewColumnConfig Settings
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
        AddingBoard NewBoardConfig.default settings_



-- UTILITIES


boardConfigs : SettingsState -> SafeZipper BoardConfig
boardConfigs settingsState =
    Settings.boardConfigs <| settings settingsState


settings : SettingsState -> Settings
settings settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            settings_

        AddingColumn _ settings_ ->
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

        AddingColumn _ settings_ ->
            AddingBoard NewBoardConfig.default settings_

        ClosingPlugin settings_ ->
            AddingBoard NewBoardConfig.default settings_

        ClosingSettings settings_ ->
            AddingBoard NewBoardConfig.default settings_

        DeletingBoard settings_ ->
            AddingBoard NewBoardConfig.default settings_

        EditingBoard settings_ ->
            AddingBoard NewBoardConfig.default settings_

        EditingGlobalSettings settings_ ->
            AddingBoard NewBoardConfig.default settings_


addColumnRequested : SettingsState -> SettingsState
addColumnRequested settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            AddingColumn (NewColumnConfig "" "") settings_

        AddingColumn _ _ ->
            settingsState

        ClosingPlugin settings_ ->
            AddingColumn (NewColumnConfig "" "") settings_

        ClosingSettings settings_ ->
            AddingColumn (NewColumnConfig "" "") settings_

        DeletingBoard settings_ ->
            AddingColumn (NewColumnConfig "" "") settings_

        EditingBoard settings_ ->
            AddingColumn (NewColumnConfig "" "") settings_

        EditingGlobalSettings settings_ ->
            AddingColumn (NewColumnConfig "" "") settings_


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ settings_ ->
            if Settings.hasAnyBordsConfigured settings_ then
                init settings_

            else
                ClosingPlugin settings_

        AddingColumn _ settings_ ->
            EditingBoard settings_

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


confirmAddBoard : DefaultColumnNames -> SettingsState -> SettingsState
confirmAddBoard defaultColumnNames settingsState =
    case settingsState of
        AddingBoard c settings_ ->
            EditingBoard <|
                Settings.cleanupNames <|
                    Settings.addBoard defaultColumnNames c settings_

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

        AddingColumn _ settings_ ->
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

        AddingColumn _ settings_ ->
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

        AddingColumn _ settings_ ->
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


moveBoard : String -> BeaconPosition -> SettingsState -> SettingsState
moveBoard draggedId beaconPosition settingsState =
    mapSettings (Settings.moveBoard draggedId beaconPosition) settingsState



-- MAPPING


mapBoardBeingAdded : (NewBoardConfig -> NewBoardConfig) -> SettingsState -> SettingsState
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



-- PRIVATE


mapSettings : (Settings -> Settings) -> SettingsState -> SettingsState
mapSettings fn settingsState =
    case settingsState of
        AddingBoard config settings_ ->
            AddingBoard config (fn settings_)

        AddingColumn config settings_ ->
            AddingColumn config (fn settings_)

        ClosingPlugin settings_ ->
            ClosingPlugin (fn settings_)

        ClosingSettings settings_ ->
            ClosingSettings (fn settings_)

        DeletingBoard settings_ ->
            DeletingBoard (fn settings_)

        EditingBoard settings_ ->
            EditingBoard (fn settings_)

        EditingGlobalSettings settings_ ->
            EditingGlobalSettings (fn settings_)
