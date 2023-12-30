module Form.SettingsState exposing
    ( SettingsState(..)
    , addBoardConfirmed
    , addBoardRequested
    , addColumnConfirmed
    , addColumnRequested
    , boardConfigs
    , cancelCurrentState
    , deleteBoardRequested
    , deleteColumnRequested
    , deleteConfirmed
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
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition exposing (BeaconPosition)
import Form.BoardConfig exposing (BoardConfigForm)
import Form.Columns exposing (ColumnsForm)
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.SafeDecoder as SD
import Form.Select exposing (Option)
import Form.Settings as SettingsForm exposing (SettingsForm)
import List.Extra as LE
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)



-- TYPES


type SettingsState
    = AddingBoard NewBoardForm SettingsForm
    | AddingColumn NewColumnForm SettingsForm
    | ClosingPlugin SettingsForm
    | ClosingSettings SettingsForm
    | DeletingBoard SettingsForm
    | DeletingColumn Int SettingsForm
    | EditingBoard SettingsForm
    | EditingGlobalSettings SettingsForm



-- CREATE


init : Settings -> SettingsState
init settings_ =
    let
        settingsForm : SettingsForm
        settingsForm =
            SettingsForm.init settings_
    in
    if SettingsForm.hasAnyBordsConfigured settingsForm then
        EditingBoard settingsForm

    else
        AddingBoard NewBoardForm.default settingsForm



-- INFO


boardConfigs : SettingsState -> SafeZipper BoardConfig
boardConfigs settingsState =
    Settings.boardConfigs <| settings settingsState


settings : SettingsState -> Settings
settings settingsState =
    let
        settingsFromForm : SettingsForm -> Settings
        settingsFromForm settingsForm_ =
            SD.run SettingsForm.safeDecoder settingsForm_
                |> Result.withDefault Settings.default
    in
    case settingsState of
        AddingBoard _ settingsForm_ ->
            settingsFromForm settingsForm_

        AddingColumn _ settingsForm_ ->
            settingsFromForm settingsForm_

        ClosingPlugin settingsForm_ ->
            settingsFromForm settingsForm_

        ClosingSettings settingsForm_ ->
            settingsFromForm settingsForm_

        DeletingBoard settingsForm_ ->
            settingsFromForm settingsForm_

        DeletingColumn _ settingsForm_ ->
            settingsFromForm settingsForm_

        EditingBoard settingsForm_ ->
            settingsFromForm settingsForm_

        EditingGlobalSettings settingsForm_ ->
            settingsFromForm settingsForm_



-- TRANSFORM


addBoardConfirmed : DefaultColumnNames -> SettingsState -> SettingsState
addBoardConfirmed defaultColumnNames settingsState =
    case settingsState of
        AddingBoard c settingsForm_ ->
            EditingBoard <| SettingsForm.addBoard defaultColumnNames c settingsForm_

        _ ->
            settingsState


addBoardRequested : SettingsState -> SettingsState
addBoardRequested settingsState =
    case settingsState of
        AddingBoard _ _ ->
            settingsState

        AddingColumn _ settingsForm_ ->
            AddingBoard NewBoardForm.default settingsForm_

        ClosingPlugin settingsForm_ ->
            AddingBoard NewBoardForm.default settingsForm_

        ClosingSettings settingsForm_ ->
            AddingBoard NewBoardForm.default settingsForm_

        DeletingBoard settingsForm_ ->
            AddingBoard NewBoardForm.default settingsForm_

        DeletingColumn _ settingsForm_ ->
            AddingBoard NewBoardForm.default settingsForm_

        EditingBoard settingsForm_ ->
            AddingBoard NewBoardForm.default settingsForm_

        EditingGlobalSettings settingsForm_ ->
            AddingBoard NewBoardForm.default settingsForm_


addColumnConfirmed : SettingsState -> SettingsState
addColumnConfirmed settingsState =
    case settingsState of
        AddingColumn c settingsForm_ ->
            EditingBoard <| SettingsForm.addColumn c settingsForm_

        _ ->
            settingsState


addColumnRequested : SettingsState -> SettingsState
addColumnRequested settingsState =
    let
        columnsForm : SettingsForm -> Maybe ColumnsForm
        columnsForm settingsForm_ =
            settingsForm_
                |> SettingsForm.boardConfigForms
                |> SafeZipper.current
                |> Maybe.map .columnsForm

        optionsForSelect : SettingsForm -> List Option
        optionsForSelect settingsForm_ =
            case columnsForm settingsForm_ of
                Just form ->
                    NewColumnForm.optionsForSelect form (NewColumnForm "" "")

                Nothing ->
                    []

        selectedOption : SettingsForm -> String
        selectedOption settingsForm_ =
            optionsForSelect settingsForm_
                |> LE.find .isSelected
                |> Maybe.map .value
                |> Maybe.withDefault "dated"
    in
    case settingsState of
        AddingBoard _ settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_

        AddingColumn _ settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_

        ClosingPlugin settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_

        ClosingSettings settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_

        DeletingBoard settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_

        DeletingColumn _ settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_

        EditingBoard settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_

        EditingGlobalSettings settingsForm_ ->
            AddingColumn (NewColumnForm "" <| selectedOption settingsForm_) settingsForm_


cancelCurrentState : SettingsState -> SettingsState
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard _ settingsForm_ ->
            if SettingsForm.hasAnyBordsConfigured settingsForm_ then
                EditingBoard settingsForm_

            else
                ClosingPlugin settingsForm_

        AddingColumn _ settingsForm_ ->
            EditingBoard settingsForm_

        ClosingPlugin settingsForm_ ->
            ClosingPlugin settingsForm_

        ClosingSettings settingsForm_ ->
            ClosingSettings settingsForm_

        DeletingBoard settingsForm_ ->
            EditingBoard settingsForm_

        DeletingColumn _ settingsForm_ ->
            EditingBoard settingsForm_

        EditingBoard settingsForm_ ->
            ClosingSettings settingsForm_

        EditingGlobalSettings settingsForm_ ->
            ClosingSettings settingsForm_


deleteBoardRequested : SettingsState -> SettingsState
deleteBoardRequested settingsState =
    case settingsState of
        AddingBoard _ settingsForm_ ->
            DeletingBoard settingsForm_

        AddingColumn _ settingsForm_ ->
            DeletingBoard settingsForm_

        ClosingPlugin settingsForm_ ->
            DeletingBoard settingsForm_

        ClosingSettings settingsForm_ ->
            DeletingBoard settingsForm_

        DeletingBoard _ ->
            settingsState

        DeletingColumn _ settingsForm_ ->
            DeletingBoard settingsForm_

        EditingBoard settingsForm_ ->
            DeletingBoard settingsForm_

        EditingGlobalSettings settingsForm_ ->
            DeletingBoard settingsForm_


deleteColumnRequested : Int -> SettingsState -> SettingsState
deleteColumnRequested index settingsState =
    case settingsState of
        AddingBoard _ settingsForm_ ->
            DeletingColumn index settingsForm_

        AddingColumn _ settingsForm_ ->
            DeletingColumn index settingsForm_

        ClosingPlugin settingsForm_ ->
            DeletingColumn index settingsForm_

        ClosingSettings settingsForm_ ->
            DeletingColumn index settingsForm_

        DeletingBoard settingsForm_ ->
            DeletingColumn index settingsForm_

        DeletingColumn _ settingsForm_ ->
            DeletingColumn index settingsForm_

        EditingBoard settingsForm_ ->
            DeletingColumn index settingsForm_

        EditingGlobalSettings settingsForm_ ->
            DeletingColumn index settingsForm_


deleteConfirmed : SettingsState -> SettingsState
deleteConfirmed settingsState =
    case settingsState of
        DeletingBoard settingsForm_ ->
            let
                newSettingsForm : SettingsForm
                newSettingsForm =
                    SettingsForm.deleteCurrentBoard settingsForm_
            in
            if SettingsForm.hasAnyBordsConfigured newSettingsForm then
                EditingBoard newSettingsForm

            else
                AddingBoard NewBoardForm.default newSettingsForm

        DeletingColumn index settingsForm_ ->
            EditingBoard (SettingsForm.deleteColumn index settingsForm_)

        _ ->
            settingsState


editBoardAt : Int -> SettingsState -> SettingsState
editBoardAt index settingsState =
    case settingsState of
        AddingBoard _ settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)

        AddingColumn _ settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)

        ClosingPlugin settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)

        ClosingSettings settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)

        DeletingBoard settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)

        DeletingColumn _ settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)

        EditingBoard settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)

        EditingGlobalSettings settingsForm_ ->
            EditingBoard
                (SettingsForm.switchToBoard index settingsForm_)


editGlobalSettings : SettingsState -> SettingsState
editGlobalSettings settingsState =
    case settingsState of
        AddingBoard _ settingsForm_ ->
            EditingGlobalSettings settingsForm_

        AddingColumn _ settingsForm_ ->
            EditingGlobalSettings settingsForm_

        ClosingPlugin settingsForm_ ->
            EditingGlobalSettings settingsForm_

        ClosingSettings settingsForm_ ->
            EditingGlobalSettings settingsForm_

        DeletingBoard settingsForm_ ->
            EditingGlobalSettings settingsForm_

        DeletingColumn _ settingsForm_ ->
            EditingGlobalSettings settingsForm_

        EditingBoard settingsForm_ ->
            EditingGlobalSettings settingsForm_

        EditingGlobalSettings _ ->
            settingsState


moveBoard : String -> BeaconPosition -> SettingsState -> SettingsState
moveBoard draggedId beaconPosition settingsState =
    mapSettingsForm (SettingsForm.moveBoard draggedId beaconPosition) settingsState


moveColumn : String -> BeaconPosition -> SettingsState -> SettingsState
moveColumn draggedId beaconPosition settingsState =
    mapSettingsForm (SettingsForm.moveColumn draggedId beaconPosition) settingsState



-- MAPPING


mapBoardBeingAdded : (NewBoardForm -> NewBoardForm) -> SettingsState -> SettingsState
mapBoardBeingAdded fn settingsState =
    case settingsState of
        AddingBoard c settingsForm_ ->
            AddingBoard (fn c) settingsForm_

        _ ->
            settingsState


mapBoardBeingEdited : (BoardConfigForm -> BoardConfigForm) -> SettingsState -> SettingsState
mapBoardBeingEdited fn settingsState =
    case settingsState of
        EditingBoard settingsForm_ ->
            EditingBoard (SettingsForm.mapCurrentBoard fn settingsForm_)

        _ ->
            settingsState


mapColumnBeingAdded : (NewColumnForm -> NewColumnForm) -> SettingsState -> SettingsState
mapColumnBeingAdded fn settingsState =
    case settingsState of
        AddingColumn c settingsForm_ ->
            AddingColumn (fn c) settingsForm_

        _ ->
            settingsState


mapCurrentColumnsForm : (ColumnsForm -> ColumnsForm) -> SettingsState -> SettingsState
mapCurrentColumnsForm fn settingsState =
    case settingsState of
        EditingBoard settingsForm_ ->
            EditingBoard (SettingsForm.mapCurrentColumnsForm fn settingsForm_)

        _ ->
            settingsState


mapGlobalSettings : (SettingsForm -> SettingsForm) -> SettingsState -> SettingsState
mapGlobalSettings fn settingsState =
    case settingsState of
        EditingGlobalSettings settingsForm_ ->
            EditingGlobalSettings (fn settingsForm_)

        _ ->
            settingsState



-- PRIVATE


mapSettingsForm : (SettingsForm -> SettingsForm) -> SettingsState -> SettingsState
mapSettingsForm fn settingsState =
    case settingsState of
        AddingBoard config settingsForm_ ->
            AddingBoard config (fn settingsForm_)

        AddingColumn config settingsForm_ ->
            AddingColumn config (fn settingsForm_)

        ClosingPlugin settingsForm_ ->
            ClosingPlugin (fn settingsForm_)

        ClosingSettings settingsForm_ ->
            ClosingSettings (fn settingsForm_)

        DeletingBoard settingsForm_ ->
            DeletingBoard (fn settingsForm_)

        DeletingColumn index settingsForm_ ->
            DeletingColumn index (fn settingsForm_)

        EditingBoard settingsForm_ ->
            EditingBoard (fn settingsForm_)

        EditingGlobalSettings settingsForm_ ->
            EditingGlobalSettings (fn settingsForm_)
