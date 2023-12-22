module SettingsState exposing
    ( SettingsState(..)
    , addBoardRequested
    , addColumnRequested
    , boardConfigs
    , cancelCurrentState
    , confirmAddBoard
    , confirmAddColumn
    , confirmDelete
    , deleteBoardRequested
    , deleteColumnRequested
    , editBoardAt
    , editGlobalSettings
    , init
    , mapBoardBeingAdded
    , mapBoardBeingEdited
    , mapColumnBeingAdded
    , mapGlobalSettings
    , moveBoard
    , moveColumn
    , settings
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns, OptionsForSelect)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition exposing (BeaconPosition)
import Form.Column as FormColumn
import GlobalSettings exposing (GlobalSettings)
import List.Extra as LE
import NewBoardConfig exposing (NewBoardConfig)
import NewColumnConfig exposing (NewColumnConfig)
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)



-- TYPES


type SettingsState
    = AddingBoard NewBoardConfig Settings (List FormColumn.Form)
    | AddingColumn NewColumnConfig Settings (List FormColumn.Form)
    | ClosingPlugin Settings (List FormColumn.Form)
    | ClosingSettings Settings (List FormColumn.Form)
    | DeletingBoard Settings (List FormColumn.Form)
    | DeletingColumn Int Settings (List FormColumn.Form)
    | EditingBoard Settings (List FormColumn.Form)
    | EditingGlobalSettings Settings (List FormColumn.Form)



-- CREATE


init : Settings -> SettingsState
init settings_ =
    if Settings.hasAnyBordsConfigured settings_ then
        EditingBoard settings_ (formColumnsFromSettings settings_)

    else
        AddingBoard NewBoardConfig.default settings_ []



-- UTILITIES


boardConfigs : SettingsState -> SafeZipper BoardConfig
boardConfigs settingsState =
    Settings.boardConfigs <| settings settingsState


settings : SettingsState -> Settings
settings settingsState =
    case settingsState of
        AddingBoard _ settings_ formColumns ->
            settings_

        AddingColumn _ settings_ formColumns ->
            settings_

        ClosingPlugin settings_ formColumns ->
            settings_

        ClosingSettings settings_ formColumns ->
            settings_

        DeletingBoard settings_ formColumns ->
            settings_

        DeletingColumn _ settings_ formColumns ->
            settings_

        EditingBoard settings_ formColumns ->
            settings_

        EditingGlobalSettings settings_ formColumns ->
            settings_



-- TRANSFORM


addBoardRequested : SettingsState -> SettingsState
addBoardRequested settingsState =
    case settingsState of
        AddingBoard _ _ formColumns ->
            settingsState

        AddingColumn _ settings_ formColumns ->
            AddingBoard NewBoardConfig.default settings_ formColumns

        ClosingPlugin settings_ formColumns ->
            AddingBoard NewBoardConfig.default settings_ formColumns

        ClosingSettings settings_ formColumns ->
            AddingBoard NewBoardConfig.default settings_ formColumns

        DeletingBoard settings_ formColumns ->
            AddingBoard NewBoardConfig.default settings_ formColumns

        DeletingColumn _ settings_ formColumns ->
            AddingBoard NewBoardConfig.default settings_ formColumns

        EditingBoard settings_ formColumns ->
            AddingBoard NewBoardConfig.default settings_ formColumns

        EditingGlobalSettings settings_ formColumns ->
            AddingBoard NewBoardConfig.default settings_ formColumns


addColumnRequested : SettingsState -> SettingsState
addColumnRequested settingsState =
    let
        columns : Columns
        columns =
            settingsState
                |> settings
                |> Settings.boardConfigs
                |> SafeZipper.current
                |> Maybe.map BoardConfig.columns
                |> Maybe.withDefault Columns.empty

        optionsForSelect : List OptionsForSelect
        optionsForSelect =
            Columns.optionsForSelect columns (NewColumnConfig "" "")

        selectedOption : String
        selectedOption =
            optionsForSelect
                |> LE.find .isSelected
                |> Maybe.map .value
                |> Maybe.withDefault "dated"
    in
    case settingsState of
        AddingBoard _ settings_ formColumns ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ formColumns

        AddingColumn _ _ formColumns ->
            settingsState

        ClosingPlugin settings_ formColumns ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ formColumns

        ClosingSettings settings_ formColumns ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ formColumns

        DeletingBoard settings_ formColumns ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ formColumns

        DeletingColumn _ settings_ formColumns ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ formColumns

        EditingBoard settings_ formColumns ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ formColumns

        EditingGlobalSettings settings_ formColumns ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ formColumns


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ settings_ formColumns ->
            if Settings.hasAnyBordsConfigured settings_ then
                init settings_

            else
                ClosingPlugin settings_ formColumns

        AddingColumn _ settings_ formColumns ->
            EditingBoard settings_ formColumns

        ClosingPlugin settings_ formColumns ->
            ClosingPlugin settings_ formColumns

        ClosingSettings settings_ formColumns ->
            ClosingSettings settings_ formColumns

        DeletingBoard settings_ formColumns ->
            EditingBoard settings_ formColumns

        DeletingColumn _ settings_ formColumns ->
            EditingBoard settings_ formColumns

        EditingBoard settings_ formColumns ->
            ClosingSettings settings_ formColumns

        EditingGlobalSettings settings_ formColumns ->
            ClosingSettings settings_ formColumns


confirmAddBoard : DefaultColumnNames -> SettingsState -> SettingsState
confirmAddBoard defaultColumnNames settingsState =
    case settingsState of
        AddingBoard c settings_ formColumns ->
            Settings.addBoard defaultColumnNames c settings_
                |> Settings.cleanupNames
                |> (\s -> EditingBoard s formColumns)

        _ ->
            settingsState


confirmAddColumn : DefaultColumnNames -> SettingsState -> SettingsState
confirmAddColumn defaultColumnNames settingsState =
    case settingsState of
        AddingColumn c settings_ formColumns ->
            Settings.addColumn defaultColumnNames c settings_
                |> (\s -> EditingBoard s formColumns)

        _ ->
            settingsState


confirmDelete : SettingsState -> SettingsState
confirmDelete settingsState =
    case settingsState of
        DeletingBoard settings_ formColumns ->
            init (Settings.deleteCurrentBoard settings_)

        DeletingColumn index settings_ formColumns ->
            EditingBoard (Settings.deleteColumn index settings_) formColumns

        _ ->
            settingsState


deleteBoardRequested : SettingsState -> SettingsState
deleteBoardRequested settingsState =
    case settingsState of
        AddingBoard _ settings_ formColumns ->
            DeletingBoard settings_ formColumns

        AddingColumn _ settings_ formColumns ->
            DeletingBoard settings_ formColumns

        ClosingPlugin settings_ formColumns ->
            DeletingBoard settings_ formColumns

        ClosingSettings settings_ formColumns ->
            DeletingBoard settings_ formColumns

        DeletingBoard _ formColumns ->
            settingsState

        DeletingColumn _ settings_ formColumns ->
            DeletingBoard settings_ formColumns

        EditingBoard settings_ formColumns ->
            DeletingBoard settings_ formColumns

        EditingGlobalSettings settings_ formColumns ->
            DeletingBoard settings_ formColumns


deleteColumnRequested : Int -> SettingsState -> SettingsState
deleteColumnRequested index settingsState =
    case settingsState of
        AddingBoard _ settings_ formColumns ->
            DeletingColumn index settings_ formColumns

        AddingColumn _ settings_ formColumns ->
            DeletingColumn index settings_ formColumns

        ClosingPlugin settings_ formColumns ->
            DeletingColumn index settings_ formColumns

        ClosingSettings settings_ formColumns ->
            DeletingColumn index settings_ formColumns

        DeletingBoard settings_ formColumns ->
            DeletingColumn index settings_ formColumns

        DeletingColumn _ settings_ formColumns ->
            DeletingColumn index settings_ formColumns

        EditingBoard settings_ formColumns ->
            DeletingColumn index settings_ formColumns

        EditingGlobalSettings settings_ formColumns ->
            DeletingColumn index settings_ formColumns


editBoardAt : Int -> SettingsState -> SettingsState
editBoardAt index settingsState =
    let
        changeToEditingBoard : Settings -> SettingsState
        changeToEditingBoard settings_ =
            let
                newSettings =
                    Settings.switchToBoard index settings_
            in
            EditingBoard newSettings (formColumnsFromSettings newSettings)
    in
    case settingsState of
        AddingBoard _ settings_ formColumns ->
            changeToEditingBoard settings_

        AddingColumn _ settings_ formColumns ->
            changeToEditingBoard settings_

        ClosingPlugin settings_ formColumns ->
            changeToEditingBoard settings_

        ClosingSettings settings_ formColumns ->
            changeToEditingBoard settings_

        DeletingBoard settings_ formColumns ->
            changeToEditingBoard settings_

        DeletingColumn _ settings_ formColumns ->
            changeToEditingBoard settings_

        EditingBoard settings_ formColumns ->
            changeToEditingBoard settings_

        EditingGlobalSettings settings_ formColumns ->
            changeToEditingBoard settings_


editGlobalSettings : SettingsState -> SettingsState
editGlobalSettings settingsState =
    case settingsState of
        AddingBoard _ settings_ formColumns ->
            EditingGlobalSettings settings_ formColumns

        AddingColumn _ settings_ formColumns ->
            EditingGlobalSettings settings_ formColumns

        ClosingPlugin settings_ formColumns ->
            EditingGlobalSettings settings_ formColumns

        ClosingSettings settings_ formColumns ->
            EditingGlobalSettings settings_ formColumns

        DeletingBoard settings_ formColumns ->
            EditingGlobalSettings settings_ formColumns

        DeletingColumn _ settings_ formColumns ->
            EditingGlobalSettings settings_ formColumns

        EditingBoard settings_ formColumns ->
            EditingGlobalSettings settings_ formColumns

        EditingGlobalSettings _ formColumns ->
            settingsState


moveBoard : String -> BeaconPosition -> SettingsState -> SettingsState
moveBoard draggedId beaconPosition settingsState =
    mapSettings (Settings.moveBoard draggedId beaconPosition) settingsState


moveColumn : String -> BeaconPosition -> SettingsState -> SettingsState
moveColumn draggedId beaconPosition settingsState =
    mapSettings (Settings.moveColumn draggedId beaconPosition) settingsState



-- MAPPING


mapBoardBeingAdded : (NewBoardConfig -> NewBoardConfig) -> SettingsState -> SettingsState
mapBoardBeingAdded fn settingsState =
    case settingsState of
        AddingBoard c settings_ formColumns ->
            AddingBoard (fn c) settings_ formColumns

        _ ->
            settingsState


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingEdited fn settingsState =
    case settingsState of
        EditingBoard settings_ formColumns ->
            EditingBoard (Settings.updateCurrentBoard fn settings_) formColumns

        _ ->
            settingsState


mapColumnBeingAdded : (NewColumnConfig -> NewColumnConfig) -> SettingsState -> SettingsState
mapColumnBeingAdded fn settingsState =
    case settingsState of
        AddingColumn c settings_ formColumns ->
            AddingColumn (fn c) settings_ formColumns

        _ ->
            settingsState


mapGlobalSettings : (GlobalSettings -> GlobalSettings) -> SettingsState -> SettingsState
mapGlobalSettings fn settingsState =
    case settingsState of
        EditingGlobalSettings settings_ formColumns ->
            EditingGlobalSettings (Settings.mapGlobalSettings fn settings_) formColumns

        _ ->
            settingsState



-- PRIVATE


formColumnsFromSettings : Settings -> List FormColumn.Form
formColumnsFromSettings settings_ =
    settings_
        |> Settings.boardConfigs
        |> SafeZipper.current
        |> Maybe.map BoardConfig.columns
        |> Maybe.map Columns.toList
        |> Maybe.withDefault []
        |> List.map FormColumn.init


mapSettings : (Settings -> Settings) -> SettingsState -> SettingsState
mapSettings fn settingsState =
    case settingsState of
        AddingBoard config settings_ formColumns ->
            AddingBoard config (fn settings_) formColumns

        AddingColumn config settings_ formColumns ->
            AddingColumn config (fn settings_) formColumns

        ClosingPlugin settings_ formColumns ->
            ClosingPlugin (fn settings_) formColumns

        ClosingSettings settings_ formColumns ->
            ClosingSettings (fn settings_) formColumns

        DeletingBoard settings_ formColumns ->
            DeletingBoard (fn settings_) formColumns

        DeletingColumn index settings_ formColumns ->
            DeletingColumn index (fn settings_) formColumns

        EditingBoard settings_ formColumns ->
            EditingBoard (fn settings_) formColumns

        EditingGlobalSettings settings_ formColumns ->
            EditingGlobalSettings (fn settings_) formColumns
