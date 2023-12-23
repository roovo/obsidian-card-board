module Form.Columns exposing
    ( Form
    , decoder
    , empty
    , find
    , init
    , updateColumnName
    )

import Columns exposing (Columns)
import Form.Column as ColumnForm
import Form.Decoder as FD
import List.Extra as LE



-- TYPES


type alias Form =
    { columnForms : List ColumnForm.Form }



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



-- DECODER


decoder : FD.Decoder Form ( Int, ColumnForm.Error ) Columns
decoder =
    FD.listOf ColumnForm.decoder
        |> FD.lift .columnForms
        |> FD.map Columns.fromList



-- INFO


find : (ColumnForm.Form -> Bool) -> Form -> Maybe ColumnForm.Form
find fn form =
    LE.find fn form.columnForms



-- MODIFICATION


updateColumnName : Int -> String -> Form -> Form
updateColumnName index newName form =
    { form | columnForms = LE.updateAt index (ColumnForm.updateName newName) form.columnForms }
