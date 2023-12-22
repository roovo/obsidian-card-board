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
import Form.BoardConfigs as BoardConfigsForm
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


settings : SettingsState -> Settings
settings settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm ->
            settings_

        AddingColumn _ settings_ boardConfigsForm ->
            settings_

        ClosingPlugin settings_ boardConfigsForm ->
            settings_

        ClosingSettings settings_ boardConfigsForm ->
            settings_

        DeletingBoard settings_ boardConfigsForm ->
            settings_

        DeletingColumn _ settings_ boardConfigsForm ->
            settings_

        EditingBoard settings_ boardConfigsForm ->
            settings_

        EditingGlobalSettings settings_ boardConfigsForm ->
            settings_



-- TRANSFORM


addBoardRequested : SettingsState -> SettingsState
addBoardRequested settingsState =
    case settingsState of
        AddingBoard _ _ boardConfigsForm ->
            settingsState

        AddingColumn _ settings_ boardConfigsForm ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm

        ClosingPlugin settings_ boardConfigsForm ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm

        ClosingSettings settings_ boardConfigsForm ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm

        DeletingBoard settings_ boardConfigsForm ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm

        DeletingColumn _ settings_ boardConfigsForm ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm

        EditingBoard settings_ boardConfigsForm ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm

        EditingGlobalSettings settings_ boardConfigsForm ->
            AddingBoard NewBoardConfig.default settings_ boardConfigsForm


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
        AddingBoard _ settings_ boardConfigsForm ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm

        AddingColumn _ _ boardConfigsForm ->
            settingsState

        ClosingPlugin settings_ boardConfigsForm ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm

        ClosingSettings settings_ boardConfigsForm ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm

        DeletingBoard settings_ boardConfigsForm ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm

        DeletingColumn _ settings_ boardConfigsForm ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm

        EditingBoard settings_ boardConfigsForm ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm

        EditingGlobalSettings settings_ boardConfigsForm ->
            AddingColumn (NewColumnConfig "" selectedOption) settings_ boardConfigsForm


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm ->
            if Settings.hasAnyBordsConfigured settings_ then
                init settings_

            else
                ClosingPlugin settings_ boardConfigsForm

        AddingColumn _ settings_ boardConfigsForm ->
            EditingBoard settings_ boardConfigsForm

        ClosingPlugin settings_ boardConfigsForm ->
            ClosingPlugin settings_ boardConfigsForm

        ClosingSettings settings_ boardConfigsForm ->
            ClosingSettings settings_ boardConfigsForm

        DeletingBoard settings_ boardConfigsForm ->
            EditingBoard settings_ boardConfigsForm

        DeletingColumn _ settings_ boardConfigsForm ->
            EditingBoard settings_ boardConfigsForm

        EditingBoard settings_ boardConfigsForm ->
            ClosingSettings settings_ boardConfigsForm

        EditingGlobalSettings settings_ boardConfigsForm ->
            ClosingSettings settings_ boardConfigsForm


confirmAddBoard : DefaultColumnNames -> SettingsState -> SettingsState
confirmAddBoard defaultColumnNames settingsState =
    case settingsState of
        AddingBoard c settings_ boardConfigsForm ->
            Settings.addBoard defaultColumnNames c settings_
                |> Settings.cleanupNames
                |> (\s -> EditingBoard s boardConfigsForm)

        _ ->
            settingsState


confirmAddColumn : DefaultColumnNames -> SettingsState -> SettingsState
confirmAddColumn defaultColumnNames settingsState =
    case settingsState of
        AddingColumn c settings_ boardConfigsForm ->
            Settings.addColumn defaultColumnNames c settings_
                |> (\s -> EditingBoard s boardConfigsForm)

        _ ->
            settingsState


confirmDelete : SettingsState -> SettingsState
confirmDelete settingsState =
    case settingsState of
        DeletingBoard settings_ boardConfigsForm ->
            init (Settings.deleteCurrentBoard settings_)

        DeletingColumn index settings_ boardConfigsForm ->
            EditingBoard (Settings.deleteColumn index settings_) boardConfigsForm

        _ ->
            settingsState


deleteBoardRequested : SettingsState -> SettingsState
deleteBoardRequested settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm ->
            DeletingBoard settings_ boardConfigsForm

        AddingColumn _ settings_ boardConfigsForm ->
            DeletingBoard settings_ boardConfigsForm

        ClosingPlugin settings_ boardConfigsForm ->
            DeletingBoard settings_ boardConfigsForm

        ClosingSettings settings_ boardConfigsForm ->
            DeletingBoard settings_ boardConfigsForm

        DeletingBoard _ boardConfigsForm ->
            settingsState

        DeletingColumn _ settings_ boardConfigsForm ->
            DeletingBoard settings_ boardConfigsForm

        EditingBoard settings_ boardConfigsForm ->
            DeletingBoard settings_ boardConfigsForm

        EditingGlobalSettings settings_ boardConfigsForm ->
            DeletingBoard settings_ boardConfigsForm


deleteColumnRequested : Int -> SettingsState -> SettingsState
deleteColumnRequested index settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm

        AddingColumn _ settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm

        ClosingPlugin settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm

        ClosingSettings settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm

        DeletingBoard settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm

        DeletingColumn _ settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm

        EditingBoard settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm

        EditingGlobalSettings settings_ boardConfigsForm ->
            DeletingColumn index settings_ boardConfigsForm


editBoardAt : Int -> SettingsState -> SettingsState
editBoardAt index settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)

        AddingColumn _ settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)

        ClosingPlugin settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)

        ClosingSettings settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)

        DeletingBoard settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)

        DeletingColumn _ settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)

        EditingBoard settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)

        EditingGlobalSettings settings_ boardConfigsForm ->
            EditingBoard
                (Settings.switchToBoard index settings_)
                (BoardConfigsForm.switchToBoard index boardConfigsForm)


editGlobalSettings : SettingsState -> SettingsState
editGlobalSettings settingsState =
    case settingsState of
        AddingBoard _ settings_ boardConfigsForm ->
            EditingGlobalSettings settings_ boardConfigsForm

        AddingColumn _ settings_ boardConfigsForm ->
            EditingGlobalSettings settings_ boardConfigsForm

        ClosingPlugin settings_ boardConfigsForm ->
            EditingGlobalSettings settings_ boardConfigsForm

        ClosingSettings settings_ boardConfigsForm ->
            EditingGlobalSettings settings_ boardConfigsForm

        DeletingBoard settings_ boardConfigsForm ->
            EditingGlobalSettings settings_ boardConfigsForm

        DeletingColumn _ settings_ boardConfigsForm ->
            EditingGlobalSettings settings_ boardConfigsForm

        EditingBoard settings_ boardConfigsForm ->
            EditingGlobalSettings settings_ boardConfigsForm

        EditingGlobalSettings _ boardConfigsForm ->
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
        AddingBoard c settings_ boardConfigsForm ->
            AddingBoard (fn c) settings_ boardConfigsForm

        _ ->
            settingsState


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingEdited fn settingsState =
    case settingsState of
        EditingBoard settings_ boardConfigsForm ->
            EditingBoard (Settings.updateCurrentBoard fn settings_) boardConfigsForm

        _ ->
            settingsState


mapColumnBeingAdded : (NewColumnConfig -> NewColumnConfig) -> SettingsState -> SettingsState
mapColumnBeingAdded fn settingsState =
    case settingsState of
        AddingColumn c settings_ boardConfigsForm ->
            AddingColumn (fn c) settings_ boardConfigsForm

        _ ->
            settingsState


mapGlobalSettings : (GlobalSettings -> GlobalSettings) -> SettingsState -> SettingsState
mapGlobalSettings fn settingsState =
    case settingsState of
        EditingGlobalSettings settings_ boardConfigsForm ->
            EditingGlobalSettings (Settings.mapGlobalSettings fn settings_) boardConfigsForm

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
        AddingBoard config settings_ boardConfigsForm ->
            AddingBoard config (fn settings_) boardConfigsForm

        AddingColumn config settings_ boardConfigsForm ->
            AddingColumn config (fn settings_) boardConfigsForm

        ClosingPlugin settings_ boardConfigsForm ->
            ClosingPlugin (fn settings_) boardConfigsForm

        ClosingSettings settings_ boardConfigsForm ->
            ClosingSettings (fn settings_) boardConfigsForm

        DeletingBoard settings_ boardConfigsForm ->
            DeletingBoard (fn settings_) boardConfigsForm

        DeletingColumn index settings_ boardConfigsForm ->
            DeletingColumn index (fn settings_) boardConfigsForm

        EditingBoard settings_ boardConfigsForm ->
            EditingBoard (fn settings_) boardConfigsForm

        EditingGlobalSettings settings_ boardConfigsForm ->
            EditingGlobalSettings (fn settings_) boardConfigsForm
