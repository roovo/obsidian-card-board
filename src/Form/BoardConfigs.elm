module Form.BoardConfigs exposing
    ( Form
    , addColumn
    , columnsForms
    , decoder
    , empty
    , init
    , switchToBoard
    , updateCurrentColumnsForm
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm
import Form.Decoder as FD
import NewColumnConfig exposing (NewColumnConfig)
import SafeZipper exposing (SafeZipper)



-- TYPES


type alias Form =
    { columnsForms : SafeZipper ColumnsForm.Form
    }



-- CONSTRUCTION


init : SafeZipper BoardConfig -> Form
init boardConfigs =
    boardConfigs
        |> SafeZipper.map (ColumnsForm.init << BoardConfig.columns)
        |> Form


empty : Form
empty =
    { columnsForms = SafeZipper.empty }



-- DECODER


decoder : FD.Decoder Form ( Int, ( Int, ColumnForm.Error ) ) (List Columns)
decoder =
    FD.listOf ColumnsForm.decoder
        |> FD.lift (SafeZipper.toList << columnsForms)



-- INFO


columnsForms : Form -> SafeZipper ColumnsForm.Form
columnsForms form =
    form.columnsForms



-- MODIFICATION


addColumn : DefaultColumnNames -> NewColumnConfig -> Form -> Form
addColumn defaultColumnNames_ configToAdd form =
    { form
        | columnsForms =
            SafeZipper.mapSelectedAndRest
                (ColumnsForm.addColumn defaultColumnNames_ configToAdd)
                identity
                form.columnsForms
    }


switchToBoard : Int -> Form -> Form
switchToBoard index form =
    { form | columnsForms = SafeZipper.atIndex index form.columnsForms }


updateCurrentColumnsForm : (ColumnsForm.Form -> ColumnsForm.Form) -> Form -> Form
updateCurrentColumnsForm fn form =
    { form | columnsForms = SafeZipper.mapSelectedAndRest fn identity form.columnsForms }
