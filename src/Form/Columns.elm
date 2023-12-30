module Form.Columns exposing
    ( ColumnsForm
    , addColumn
    , deleteColumn
    , empty
    , find
    , init
    , moveColumn
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
import Form.SafeDecoder as SD
import List.Extra as LE



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



-- MODIFICATION


addColumn : Maybe ColumnForm -> ColumnsForm -> ColumnsForm
addColumn columnForm columnsForm =
    case columnForm of
        Just form ->
            ColumnsForm <| columnsForm.columnForms ++ [ form ]

        Nothing ->
            columnsForm


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
