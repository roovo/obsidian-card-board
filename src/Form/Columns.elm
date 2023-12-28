module Form.Columns exposing
    ( ColumnsForm
    , addColumn
    , deleteColumn
    , empty
    , find
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
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Form.Column as ColumnForm exposing (ColumnForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.SafeDecoder as SD
import Form.Select exposing (Option)
import List.Extra as LE
import Maybe.Extra as ME



-- TYPES


type alias ColumnsForm =
    { columnForms : List ColumnForm }



-- CONSTRUCTION


init : Columns -> ColumnsForm
init columns =
    columns
        |> Columns.toList
        |> List.map ColumnForm.init
        |> ColumnsForm


empty : ColumnsForm
empty =
    { columnForms = [] }



-- DECODER


safeDecoder : SD.Decoder ColumnsForm Columns
safeDecoder =
    SD.listOf ColumnForm.safeDecoder
        |> SD.lift .columnForms
        |> SD.map Columns.fromList



-- INFO


find : (ColumnForm -> Bool) -> ColumnsForm -> Maybe ColumnForm
find fn form =
    LE.find fn form.columnForms


optionsForSelect : ColumnsForm -> NewColumnForm -> List Option
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

        completed : List Option
        completed =
            if alreadyHasCompleted then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "completed"
                  , text = "Completed"
                  , value = "Completed"
                  }
                ]

        otherTags : List Option
        otherTags =
            if alreadyHasOtherTags then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "otherTags"
                  , text = "Other Tags"
                  , value = "OtherTags"
                  }
                ]

        undated : List Option
        undated =
            if alreadyHasUndated then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "undated"
                  , text = "Undated"
                  , value = "Undated"
                  }
                ]

        untagged : List Option
        untagged =
            if alreadyHasUntagged then
                []

            else
                [ { isSelected = newColumnConfigForm.columnType == "untagged"
                  , text = "Untagged"
                  , value = "Untagged"
                  }
                ]

        allColumns : List Option
        allColumns =
            completed
                ++ [ { isSelected = newColumnConfigForm.columnType == "dated"
                     , text = "Dated"
                     , value = "Dated"
                     }
                   ]
                ++ otherTags
                ++ [ { isSelected = newColumnConfigForm.columnType == "namedTag"
                     , text = "Tagged"
                     , value = "NamedTag"
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


addColumn : NewColumnForm -> ColumnsForm -> ColumnsForm
addColumn newColumnForm columnsForm =
    let
        newColumn : List ColumnForm
        newColumn =
            SD.run NewColumnForm.safeDecoder newColumnForm
                |> Result.toMaybe
                |> ME.join
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    ColumnsForm <| columnsForm.columnForms ++ newColumn


deleteColumn : Int -> ColumnsForm -> ColumnsForm
deleteColumn index form =
    { form | columnForms = LE.removeAt index form.columnForms }


moveColumn : String -> BeaconPosition -> ColumnsForm -> ColumnsForm
moveColumn draggedId beaconPosition form =
    form
        |> .columnForms
        |> BeaconPosition.performMove draggedId beaconPosition ColumnForm.name
        |> ColumnsForm


updateColumnName : Int -> String -> ColumnsForm -> ColumnsForm
updateColumnName index newName form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateName newName) form.columnForms }


updateCompletedColumnLimit : Int -> String -> ColumnsForm -> ColumnsForm
updateCompletedColumnLimit index newLimit form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateCompletedColumnLimit newLimit) form.columnForms }


updateDatedColumnRangeType : Int -> String -> ColumnsForm -> ColumnsForm
updateDatedColumnRangeType index newType form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateDatedColumnRangeType newType) form.columnForms }


updateDatedColumnRangeValueFrom : Int -> String -> ColumnsForm -> ColumnsForm
updateDatedColumnRangeValueFrom index newValue form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateDatedColumnRangeValueFrom newValue) form.columnForms }


updateDatedColumnRangeValueTo : Int -> String -> ColumnsForm -> ColumnsForm
updateDatedColumnRangeValueTo index newValue form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateDatedColumnRangeValueTo newValue) form.columnForms }


updateNamedTagTag : Int -> String -> ColumnsForm -> ColumnsForm
updateNamedTagTag index newName form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateNamedTagTag newName) form.columnForms }
