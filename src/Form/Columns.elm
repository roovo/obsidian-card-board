module Form.Columns exposing
    ( Form
    , OptionsForSelect
    , addColumn
    , decoder
    , deleteColumn
    , empty
    , find
    , fromNewBoardForm
    , init
    , moveColumn
    , optionsForSelect
    , safeDecoder
    , updateColumnName
    , updateCompletedColumnLimit
    , updateDatedColumnRangeType
    , updateDatedColumnRangeValueFrom
    , updateDatedColumnRangeValueTo
    , updateNamedTagTag
    )

import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Form.Column as ColumnForm
import Form.Decoder as FD
import Form.NewBoard exposing (NewBoardForm)
import Form.NewColumn exposing (NewColumnForm)
import Form.SafeDecoder as SD
import List.Extra as LE



-- TYPES


type alias Form =
    { columnForms : List ColumnForm.Form }


type alias OptionsForSelect =
    { isSelected : Bool
    , text : String
    , value : String
    }



-- CONSTRUCTION


init : Columns -> Form
init columns =
    columns
        |> Columns.toList
        |> List.map ColumnForm.init
        |> Form


empty : Form
empty =
    { columnForms = [] }


fromNewBoardForm : DefaultColumnNames -> NewBoardForm -> Form
fromNewBoardForm defaultColumnNames newBoardConfigForm =
    case newBoardConfigForm.boardType of
        "dateBoard" ->
            Form
                [ ColumnForm.UndatedColumnForm { name = DefaultColumnNames.nameFor "undated" defaultColumnNames }
                , ColumnForm.DatedColumnForm { name = DefaultColumnNames.nameFor "today" defaultColumnNames, rangeType = "Before", from = "", to = "1" }
                , ColumnForm.DatedColumnForm { name = DefaultColumnNames.nameFor "tomorrow" defaultColumnNames, rangeType = "Between", from = "1", to = "1" }
                , ColumnForm.DatedColumnForm { name = DefaultColumnNames.nameFor "future" defaultColumnNames, rangeType = "After", from = "1", to = "" }
                , ColumnForm.CompletedColumnForm { name = DefaultColumnNames.nameFor "completed" defaultColumnNames, limit = "10" }
                ]

        "tagBoard" ->
            Form
                [ ColumnForm.UntaggedColumnForm { name = DefaultColumnNames.nameFor "untagged" defaultColumnNames }
                , ColumnForm.OtherTagsColumnForm { name = DefaultColumnNames.nameFor "otherTags" defaultColumnNames }
                , ColumnForm.CompletedColumnForm { name = DefaultColumnNames.nameFor "completed" defaultColumnNames, limit = "10" }
                ]

        _ ->
            Form []



-- DECODER


decoder : FD.Decoder Form ( Int, ColumnForm.Error ) Columns
decoder =
    FD.listOf ColumnForm.decoder
        |> FD.lift .columnForms
        |> FD.map Columns.fromList


safeDecoder : SD.Decoder Form Columns
safeDecoder =
    SD.listOf ColumnForm.safeDecoder
        |> SD.lift .columnForms
        |> SD.map Columns.fromList



-- INFO


find : (ColumnForm.Form -> Bool) -> Form -> Maybe ColumnForm.Form
find fn form =
    LE.find fn form.columnForms


optionsForSelect : Form -> NewColumnForm -> List OptionsForSelect
optionsForSelect form newColumnConfigForm =
    let
        alreadyHasCompleted : Bool
        alreadyHasCompleted =
            form
                |> .columnForms
                |> List.any ColumnForm.isCompleted

        alreadyHasOtherTags : Bool
        alreadyHasOtherTags =
            form
                |> .columnForms
                |> List.any ColumnForm.isOtherTags

        alreadyHasUndated : Bool
        alreadyHasUndated =
            form
                |> .columnForms
                |> List.any ColumnForm.isUndated

        alreadyHasUntagged : Bool
        alreadyHasUntagged =
            form
                |> .columnForms
                |> List.any ColumnForm.isUntagged

        completed : List OptionsForSelect
        completed =
            if alreadyHasCompleted then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "completed"
                  , text = "Completed"
                  , value = "completed"
                  }
                ]

        otherTags : List OptionsForSelect
        otherTags =
            if alreadyHasOtherTags then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "otherTags"
                  , text = "Other Tags"
                  , value = "otherTags"
                  }
                ]

        undated : List OptionsForSelect
        undated =
            if alreadyHasUndated then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "undated"
                  , text = "Undated"
                  , value = "undated"
                  }
                ]

        untagged : List OptionsForSelect
        untagged =
            if alreadyHasUntagged then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "untagged"
                  , text = "Untagged"
                  , value = "untagged"
                  }
                ]

        allColumns : List OptionsForSelect
        allColumns =
            completed
                ++ [ { isSelected = newColumnConfigForm.columnType == "dated"
                     , text = "Dated"
                     , value = "dated"
                     }
                   ]
                ++ otherTags
                ++ [ { isSelected = newColumnConfigForm.columnType == "namedTag"
                     , text = "Tagged"
                     , value = "namedTag"
                     }
                   ]
                ++ undated
                ++ untagged
    in
    if List.any .isSelected allColumns then
        allColumns

    else
        allColumns
            |> LE.updateAt 0 (\ofs -> { ofs | isSelected = True })



-- MODIFICATION


addColumn : NewColumnForm -> Form -> Form
addColumn newColumnConfigForm form =
    -- let
    --     allColumns : List ColumnForm.Form
    --     allColumns =
    --         form.columnForms
    --     completedPosition : Maybe Int
    --     completedPosition =
    --         LE.findIndex ColumnForm.isCompleted allColumns
    --     newColumn : List ColumnForm.Form
    --     newColumn =
    --         ColumnForm.fromColumnConfig newColumnConfigForm
    --             |> Maybe.map List.singleton
    --             |> Maybe.withDefault []
    -- in
    -- (case completedPosition of
    --     Just position ->
    --         let
    --             ( preCompleted, completedPlus ) =
    --                 LE.splitAt position allColumns
    --         in
    --         if List.length completedPlus == 1 then
    --             preCompleted ++ newColumn ++ completedPlus
    --         else
    --             allColumns ++ newColumn
    --     Nothing ->
    --         allColumns ++ newColumn
    -- )
    --     |> Form
    form


deleteColumn : Int -> Form -> Form
deleteColumn index form =
    { form | columnForms = LE.removeAt index form.columnForms }


moveColumn : String -> BeaconPosition -> Form -> Form
moveColumn draggedId beaconPosition form =
    form
        |> .columnForms
        |> BeaconPosition.performMove draggedId beaconPosition ColumnForm.name
        |> Form


updateColumnName : Int -> String -> Form -> Form
updateColumnName index newName form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateName newName) form.columnForms }


updateCompletedColumnLimit : Int -> String -> Form -> Form
updateCompletedColumnLimit index newLimit form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateCompletedColumnLimit newLimit) form.columnForms }


updateDatedColumnRangeType : Int -> String -> Form -> Form
updateDatedColumnRangeType index newType form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateDatedColumnRangeType newType) form.columnForms }


updateDatedColumnRangeValueFrom : Int -> String -> Form -> Form
updateDatedColumnRangeValueFrom index newValue form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateDatedColumnRangeValueFrom newValue) form.columnForms }


updateDatedColumnRangeValueTo : Int -> String -> Form -> Form
updateDatedColumnRangeValueTo index newValue form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateDatedColumnRangeValueTo newValue) form.columnForms }


updateNamedTagTag : Int -> String -> Form -> Form
updateNamedTagTag index newName form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateNamedTagTag newName) form.columnForms }
