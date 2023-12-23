module SettingsState exposing
    ( SettingsState(..)
    , addBoardConfirmed
    , addBoardRequested
    , addColumnConfirmed
    , addColumnRequested
    , boardConfigs
    , cancelCurrentState
    , confirmDelete
    , deleteBoardRequested
    , deleteColumnRequested
    , editBoardAt
    , editGlobalSettings
    , init
    , mapBoardBeingAdded
    , mapBoardBeingEdited
    , mapColumnBeingAdded
    , mapCurrentColumnsForm
    , mapGlobalSettings
    , moveBoard
    , moveColumn
    , settings
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition exposing (BeaconPosition)
import Form.BoardConfigs as BoardConfigsForm
import Form.Columns as ColumnsForm exposing (OptionsForSelect)
import GlobalSettings exposing (GlobalSettings)
import List.Extra as LE
import NewBoardConfig exposing (NewBoardConfig)
import NewColumnConfig exposing (NewColumnConfig)
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)



-- TYPES


type SettingsState
    = AddingBoard NewBoardConfig Settings BoardConfigsForm.Form
    | AddingColumn NewColumnConfig Settings BoardConfigsForm.Form
    | ClosingPlugin Settings BoardConfigsForm.Form
    | ClosingSettings Settings BoardConfigsForm.Form
    | DeletingBoard Settings BoardConfigsForm.Form
    | DeletingColumn Int Settings BoardConfigsForm.Form
    | EditingBoard Settings BoardConfigsForm.Form
    | EditingGlobalSettings Settings BoardConfigsForm.Form



-- CREATE


init : Settings -> SettingsState
init settings_ =
    if Settings.hasAnyBordsConfigured settings_ then
        EditingBoard settings_ (boardConfigsFormFromSettings settings_)

    else
        AddingBoard NewBoardConfig.default settings_ BoardConfigsForm.empty



-- UTILITIES


boardConfigs : SettingsState -> SafeZipper BoardConfig
boardConfigs settingsState =
    Settings.boardConfigs <| settings settingsState


boardConfigsForm : SettingsState -> BoardConfigsForm.Form
boardConfigsForm settingsState =
    case settingsState of
        AddingBoard _ _ boardConfigsForm_ ->
            boardConfigsForm_

        AddingColumn _ _ boardConfigsForm_ ->
            boardConfigsForm_

        ClosingPlugin _ boardConfigsForm_ ->
            boardConfigsForm_

        ClosingSettings _ boardConfigsForm_ ->
            boardConfigsForm_

        DeletingBoard _ boardConfigsForm_ ->
            boardConfigsForm_

        DeletingColumn _ _ boardConfigsForm_ ->
            boardConfigsForm_

        EditingBoard _ boardConfigsForm_ ->
            boardConfigsForm_

        EditingGlobalSettings _ boardConfigsForm_ ->
            boardConfigsForm_


settings : SettingsState -> Settings
settings settingsState =
    case settingsState of
        AddingBoard _ settings_ _ ->
            settings_

        AddingColumn _ settings_ _ ->
            settings_

        ClosingPlugin settings_ _ ->
            settings_

        ClosingSettings settings_ _ ->
            settings_

        DeletingBoard settings_ _ ->
            settings_

        DeletingColumn _ settings_ _ ->
            settings_

        EditingBoard settings_ _ ->
            settings_

        EditingGlobalSettings settings_ _ ->
            settings_



-- TRANSFORM


addBoardConfirmed : DefaultColumnNames -> SettingsState -> SettingsState
addBoardConfirmed defaultColumnNames settingsState =
    case settingsState of
        AddingBoard c settings_ boardConfigsForm_ ->
            Settings.addBoard defaultColumnNames c settings_
                |> Settings.cleanupNames
                |> (\s -> EditingBoard s boardConfigsForm_)

        _ ->
            settingsState


addBoardRequested : SettingsState -> SettingsState
addBoardRequested settingsState =
    case settingsState of
        AddingBoard _ _ boardConfigsForm_ ->
            settingsState

        AddingColumn _ settings_ boardConfigsForm_ ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm_

        ClosingPlugin settings_ boardConfigsForm_ ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm_

        ClosingSettings settings_ boardConfigsForm_ ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm_

        DeletingBoard settings_ boardConfigsForm_ ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm_

        DeletingColumn _ settings_ boardConfigsForm_ ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm_

        EditingBoard settings_ boardConfigsForm_ ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm_

        EditingGlobalSettings settings_ boardConfigsForm_ ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm_


addColumnConfirmed : SettingsState -> SettingsState
addColumnConfirmed settingsState =
    case settingsState of
        AddingColumn c settings_ boardConfigsForm_ ->
            BoardConfigsForm.addColumn c boardConfigsForm_
                |> (\f -> EditingBoard settings_ f)

        _ ->
            settingsState


addColumnRequested : SettingsState -> SettingsState
addColumnRequested settingsState =
    let
        columns : ColumnsForm.Form
        columns =
            settingsState
                |> boardConfigsForm
                |> BoardConfigsForm.columnsForms
                |> SafeZipper.current
                |> Maybe.withDefault ColumnsForm.empty

        optionsForSelect : List OptionsForSelect
        optionsForSelect =
            ColumnsForm.optionsForSelect columns (NewColumnConfig "" "")

        selectedOption : String
        selectedOption =
            optionsForSelect
                |> LE.find .isSelected
                |> Maybe.map .value
                |> Maybe.withDefault "dated"
    in
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm_ ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm_

        AddingColumn _ _ boardConfigsForm_ ->
            settingsState

        ClosingPlugin settings_ boardConfigsForm_ ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm_

        ClosingSettings settings_ boardConfigsForm_ ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm_

        DeletingBoard settings_ boardConfigsForm_ ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm_

        DeletingColumn _ settings_ boardConfigsForm_ ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm_

        EditingBoard settings_ boardConfigsForm_ ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm_

        EditingGlobalSettings settings_ boardConfigsForm_ ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm_


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm_ ->
            if Settings.hasAnyBordsConfigured settings_ then
                init settings_

            else
                ClosingPlugin settings_ boardConfigsForm_

        AddingColumn _ settings_ boardConfigsForm_ ->
            EditingBoard settings_ boardConfigsForm_

        ClosingPlugin settings_ boardConfigsForm_ ->
            ClosingPlugin settings_ boardConfigsForm_

        ClosingSettings settings_ boardConfigsForm_ ->
            ClosingSettings settings_ boardConfigsForm_

        DeletingBoard settings_ boardConfigsForm_ ->
            EditingBoard settings_ boardConfigsForm_

        DeletingColumn _ settings_ boardConfigsForm_ ->
            EditingBoard settings_ boardConfigsForm_

        EditingBoard settings_ boardConfigsForm_ ->
            ClosingSettings settings_ boardConfigsForm_

        EditingGlobalSettings settings_ boardConfigsForm_ ->
            ClosingSettings settings_ boardConfigsForm_


confirmDelete : SettingsState -> SettingsState
confirmDelete settingsState =
    case settingsState of
        DeletingBoard settings_ boardConfigsForm_ ->
            init (Settings.deleteCurrentBoard settings_)

        DeletingColumn index settings_ boardConfigsForm_ ->
            EditingBoard (Settings.deleteColumn index settings_) boardConfigsForm_

        _ ->
            settingsState


deleteBoardRequested : SettingsState -> SettingsState
deleteBoardRequested settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm_ ->
            DeletingBoard settings_ boardConfigsForm_

        AddingColumn _ settings_ boardConfigsForm_ ->
            DeletingBoard settings_ boardConfigsForm_

        ClosingPlugin settings_ boardConfigsForm_ ->
            DeletingBoard settings_ boardConfigsForm_

        ClosingSettings settings_ boardConfigsForm_ ->
            DeletingBoard settings_ boardConfigsForm_

        DeletingBoard _ boardConfigsForm_ ->
            settingsState

        DeletingColumn _ settings_ boardConfigsForm_ ->
            DeletingBoard settings_ boardConfigsForm_

        EditingBoard settings_ boardConfigsForm_ ->
            DeletingBoard settings_ boardConfigsForm_

        EditingGlobalSettings settings_ boardConfigsForm_ ->
            DeletingBoard settings_ boardConfigsForm_


deleteColumnRequested : Int -> SettingsState -> SettingsState
deleteColumnRequested index settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_

        AddingColumn _ settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_

        ClosingPlugin settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_

        ClosingSettings settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_

        DeletingBoard settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_

        DeletingColumn _ settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_

        EditingBoard settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_

        EditingGlobalSettings settings_ boardConfigsForm_ ->
            DeletingColumn index settings_ boardConfigsForm_


editBoardAt : Int -> SettingsState -> SettingsState
editBoardAt index settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)

        AddingColumn _ settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)

        ClosingPlugin settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)

        ClosingSettings settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)

        DeletingBoard settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)

        DeletingColumn _ settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)

        EditingBoard settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)

        EditingGlobalSettings settings_ boardConfigsForm_ ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm_)


editGlobalSettings : SettingsState -> SettingsState
editGlobalSettings settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm_ ->
            EditingGlobalSettings settings_ boardConfigsForm_

        AddingColumn _ settings_ boardConfigsForm_ ->
            EditingGlobalSettings settings_ boardConfigsForm_

        ClosingPlugin settings_ boardConfigsForm_ ->
            EditingGlobalSettings settings_ boardConfigsForm_

        ClosingSettings settings_ boardConfigsForm_ ->
            EditingGlobalSettings settings_ boardConfigsForm_

        DeletingBoard settings_ boardConfigsForm_ ->
            EditingGlobalSettings settings_ boardConfigsForm_

        DeletingColumn _ settings_ boardConfigsForm_ ->
            EditingGlobalSettings settings_ boardConfigsForm_

        EditingBoard settings_ boardConfigsForm_ ->
            EditingGlobalSettings settings_ boardConfigsForm_

        EditingGlobalSettings _ boardConfigsForm_ ->
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
        AddingBoard c settings_ boardConfigsForm_ ->
            AddingBoard (fn c) settings_ boardConfigsForm_

        _ ->
            settingsState


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingEdited fn settingsState =
    case settingsState of
        EditingBoard settings_ boardConfigsForm_ ->
            EditingBoard (Settings.updateCurrentBoard fn settings_) boardConfigsForm_

        _ ->
            settingsState


mapColumnBeingAdded : (NewColumnConfig -> NewColumnConfig) -> SettingsState -> SettingsState
mapColumnBeingAdded fn settingsState =
    case settingsState of
        AddingColumn c settings_ boardConfigsForm_ ->
            AddingColumn (fn c) settings_ boardConfigsForm_

        _ ->
            settingsState


mapCurrentColumnsForm : (ColumnsForm.Form -> ColumnsForm.Form) -> SettingsState -> SettingsState
mapCurrentColumnsForm fn settingsState =
    case settingsState of
        EditingBoard settings_ boardConfigsForm_ ->
            EditingBoard settings_ (BoardConfigsForm.updateCurrentColumnsForm fn boardConfigsForm_)

        _ ->
            settingsState


mapGlobalSettings : (GlobalSettings -> GlobalSettings) -> SettingsState -> SettingsState
mapGlobalSettings fn settingsState =
    case settingsState of
        EditingGlobalSettings settings_ boardConfigsForm_ ->
            EditingGlobalSettings (Settings.mapGlobalSettings fn settings_) boardConfigsForm_

        _ ->
            settingsState



-- PRIVATE


boardConfigsFormFromSettings : Settings -> BoardConfigsForm.Form
boardConfigsFormFromSettings settings_ =
    settings_
        |> Settings.boardConfigs
        |> BoardConfigsForm.init


mapSettings : (Settings -> Settings) -> SettingsState -> SettingsState
mapSettings fn settingsState =
    case settingsState of
        AddingBoard config settings_ boardConfigsForm_ ->
            AddingBoard config (fn settings_) boardConfigsForm_

        AddingColumn config settings_ boardConfigsForm_ ->
            AddingColumn config (fn settings_) boardConfigsForm_

        ClosingPlugin settings_ boardConfigsForm_ ->
            ClosingPlugin (fn settings_) boardConfigsForm_

        ClosingSettings settings_ boardConfigsForm_ ->
            ClosingSettings (fn settings_) boardConfigsForm_

        DeletingBoard settings_ boardConfigsForm_ ->
            DeletingBoard (fn settings_) boardConfigsForm_

        DeletingColumn index settings_ boardConfigsForm_ ->
            DeletingColumn index (fn settings_) boardConfigsForm_

        EditingBoard settings_ boardConfigsForm_ ->
            EditingBoard (fn settings_) boardConfigsForm_

        EditingGlobalSettings settings_ boardConfigsForm_ ->
            EditingGlobalSettings (fn settings_) boardConfigsForm_
