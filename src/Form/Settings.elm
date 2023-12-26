module Form.Settings exposing
    ( Form
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
import Form.BoardConfig as BoardConfigForm
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


type alias Form =
    { boardConfigForms : SafeZipper BoardConfigForm.Form
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


init : Settings -> Form
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



-- empty : Form
-- empty =
--     { boardConfigForms = SafeZipper.empty }
-- DECODER
-- decoder : FD.Decoder Form ( Int, ( Int, ColumnForm.Error ) ) (List Columns)
-- decoder =
--     FD.listOf ColumnsForm.decoder
--         |> FD.lift (SafeZipper.toList << boardConfigForms)


safeDecoder : SD.Decoder Form Settings
safeDecoder =
    -- SD.listOf BoardConfigForm.safeDecoder
    --     |> SD.lift (SafeZipper.toList << boardConfigForms)
    SD.always Settings.default



-- INFO


boardConfigForms : Form -> SafeZipper BoardConfigForm.Form
boardConfigForms form =
    form.boardConfigForms


defaultColumnNames : Form -> DefaultColumnNames
defaultColumnNames form =
    DefaultColumnNames.default


hasAnyBordsConfigured : Form -> Bool
hasAnyBordsConfigured form =
    SafeZipper.length form.boardConfigForms /= 0



-- MODIFICATION


addBoard : DefaultColumnNames -> NewBoardForm -> Form -> Form
addBoard defaultColumnNames_ configToAdd form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.last <|
    --             SafeZipper.add
    --                 (ColumnsForm.fromNewBoardForm defaultColumnNames_ configToAdd)
    --                 form.boardConfigForms
    -- }
    form


addColumn : NewColumnForm -> Form -> Form
addColumn configToAdd form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.mapSelectedAndRest
    --             (ColumnsForm.addColumn configToAdd)
    --             identity
    --             form.boardConfigForms
    -- }
    form


deleteColumn : Int -> Form -> Form
deleteColumn index form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.mapSelectedAndRest
    --             (ColumnsForm.deleteColumn index)
    --             identity
    --             form.boardConfigForms
    -- }
    form


deleteCurrentBoard : Form -> Form
deleteCurrentBoard form =
    { form | boardConfigForms = SafeZipper.deleteCurrent form.boardConfigForms }


moveColumn : String -> BeaconPosition -> Form -> Form
moveColumn draggedId beaconPosition form =
    -- { form
    --     | boardConfigForms =
    --         SafeZipper.mapSelectedAndRest
    --             (ColumnsForm.moveColumn draggedId beaconPosition)
    --             identity
    --             form.boardConfigForms
    -- }
    form


switchToBoard : Int -> Form -> Form
switchToBoard index form =
    { form | boardConfigForms = SafeZipper.atIndex index form.boardConfigForms }



-- updateCurrentColumnsForm : (ColumnsForm.Form -> ColumnsForm.Form) -> Form -> Form
-- updateCurrentColumnsForm fn form =
--     { form | boardConfigForms = SafeZipper.mapSelectedAndRest fn identity form.boardConfigForms }
