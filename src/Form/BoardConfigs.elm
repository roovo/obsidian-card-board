module Form.BoardConfigs exposing
    ( Form
    , addBoard
    , addColumn
    , columnsForms
    , decoder
    , deleteColumn
    , empty
    , init
    , moveColumn
    , switchToBoard
    , updateCurrentColumnsForm
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm
import Form.Decoder as FD
import NewBoardConfig exposing (NewBoardConfig)
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


addBoard : DefaultColumnNames -> NewBoardConfig -> Form -> Form
addBoard defaultColumnNames_ configToAdd form =
    { form
        | columnsForms =
            SafeZipper.last <|
                SafeZipper.add
                    (ColumnsForm.fromNewBoardConfig defaultColumnNames_ configToAdd)
                    form.columnsForms
    }


addColumn : NewColumnConfig -> Form -> Form
addColumn configToAdd form =
    { form
        | columnsForms =
            SafeZipper.mapSelectedAndRest
                (ColumnsForm.addColumn configToAdd)
                identity
                form.columnsForms
    }


deleteColumn : Int -> Form -> Form
deleteColumn index form =
    { form
        | columnsForms =
            SafeZipper.mapSelectedAndRest
                (ColumnsForm.deleteColumn index)
                identity
                form.columnsForms
    }


moveColumn : String -> BeaconPosition -> Form -> Form
moveColumn draggedId beaconPosition form =
    { form
        | columnsForms =
            SafeZipper.mapSelectedAndRest
                (ColumnsForm.moveColumn draggedId beaconPosition)
                identity
                form.columnsForms
    }


switchToBoard : Int -> Form -> Form
switchToBoard index form =
    { form | columnsForms = SafeZipper.atIndex index form.columnsForms }


updateCurrentColumnsForm : (ColumnsForm.Form -> ColumnsForm.Form) -> Form -> Form
updateCurrentColumnsForm fn form =
    { form | columnsForms = SafeZipper.mapSelectedAndRest fn identity form.columnsForms }
