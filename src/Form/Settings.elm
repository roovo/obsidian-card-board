module Form.Settings exposing
    ( SettingsForm
    , addBoard
    , addColumn
    , boardConfigForms
      -- , decoder
    , defaultColumnNames
    , deleteColumn
    , deleteCurrentBoard
      -- , empty
    , hasAnyBordsConfigured
    , init
    , moveColumn
    , safeDecoder
    , switchToBoard
      -- , updateCurrentColumnsForm
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm
import Form.Decoder as FD
import Form.NewBoard exposing (NewBoardForm)
import Form.NewColumn exposing (NewColumnForm)
import Form.SafeDecoder as SD
import GlobalSettings
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)



-- TYPES


type alias SettingsForm =
    { boardConfigForms : SafeZipper BoardConfigForm
    , taskCompletionFormat : String
    , today : String
    , tomorrow : String
    , future : String
    , undated : String
    , otherTags : String
    , untagged : String
    , completed : String
    , ignoreFileNameDates : Bool
    }



-- CONSTRUCTION


init : Settings -> SettingsForm
init settings =
    let
        boardConfigForms_ =
            -- SafeZipper.map (ColumnsForm.init << BoardConfig.columns) (Settings.boardConfigs settings)
            SafeZipper.empty

        taskCompletionFormat =
            case Settings.globalSettings settings |> .taskCompletionFormat of
                GlobalSettings.NoCompletion ->
                    "NoCompletion"

                GlobalSettings.ObsidianCardBoard ->
                    "ObsidianCardBoard"

                GlobalSettings.ObsidianDataview ->
                    "ObsidianDataview"

                GlobalSettings.ObsidianTasks ->
                    "ObsidianTasks"

        globalSettings =
            Settings.globalSettings settings

        defaultColumnNames_ =
            globalSettings.defaultColumnNames
    in
    { boardConfigForms = boardConfigForms_
    , taskCompletionFormat = taskCompletionFormat
    , today = Maybe.withDefault "" defaultColumnNames_.today
    , tomorrow = Maybe.withDefault "" defaultColumnNames_.tomorrow
    , future = Maybe.withDefault "" defaultColumnNames_.future
    , undated = Maybe.withDefault "" defaultColumnNames_.undated
    , otherTags = Maybe.withDefault "" defaultColumnNames_.otherTags
    , untagged = Maybe.withDefault "" defaultColumnNames_.untagged
    , completed = Maybe.withDefault "" defaultColumnNames_.completed
    , ignoreFileNameDates = globalSettings.ignoreFileNameDates
    }



-- empty : SettingsForm
-- empty =
--     { boardConfigForms = SafeZipper.empty }
-- DECODER
-- decoder : FD.Decoder SettingsForm ( Int, ( Int, ColumnForm.Error ) ) (List Columns)
-- decoder =
--     FD.listOf ColumnsForm.decoder
--         |> FD.lift (SafeZipper.toList << boardConfigForms)


safeDecoder : SD.Decoder SettingsForm Settings
safeDecoder =
    -- SD.listOf BoardConfigForm.safeDecoder
    --     |> SD.lift (SafeZipper.toList << boardConfigForms)
    SD.always Settings.default



-- INFO


boardConfigForms : SettingsForm -> SafeZipper BoardConfigForm
boardConfigForms form =
    form.boardConfigForms


defaultColumnNames : SettingsForm -> DefaultColumnNames
defaultColumnNames form =
    DefaultColumnNames.default


hasAnyBordsConfigured : SettingsForm -> Bool
hasAnyBordsConfigured form =
    SafeZipper.length form.boardConfigForms /= 0



-- MODIFICATION


addBoard : DefaultColumnNames -> NewBoardForm -> SettingsForm -> SettingsForm
addBoard defaultColumnNames_ configToAdd form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.last <|
    --             SafeZipper.add
    --                 (ColumnsForm.fromNewBoardForm defaultColumnNames_ configToAdd)
    --                 form.boardConfigForms
    -- }
    form


addColumn : NewColumnForm -> SettingsForm -> SettingsForm
addColumn configToAdd form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.mapSelectedAndRest
    --             (ColumnsForm.addColumn configToAdd)
    --             identity
    --             form.boardConfigForms
    -- }
    form


deleteColumn : Int -> SettingsForm -> SettingsForm
deleteColumn index form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.mapSelectedAndRest
    --             (ColumnsForm.deleteColumn index)
    --             identity
    --             form.boardConfigForms
    -- }
    form


deleteCurrentBoard : SettingsForm -> SettingsForm
deleteCurrentBoard form =
    { form | boardConfigForms = SafeZipper.deleteCurrent form.boardConfigForms }


moveColumn : String -> BeaconPosition -> SettingsForm -> SettingsForm
moveColumn draggedId beaconPosition form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.mapSelectedAndRest
    --             (ColumnsForm.moveColumn draggedId beaconPosition)
    --             identity
    --             form.boardConfigForms
    -- }
    form


switchToBoard : Int -> SettingsForm -> SettingsForm
switchToBoard index form =
    { form | boardConfigForms = SafeZipper.atIndex index form.boardConfigForms }



-- updateCurrentColumnsForm : (ColumnsForm.Form -> ColumnsForm.Form) -> SettingsForm -> SettingsForm
-- updateCurrentColumnsForm fn form =
--     { form | boardConfigForms = SafeZipper.mapSelectedAndRest fn identity form.boardConfigForms }
