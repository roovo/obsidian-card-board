module Form.Column exposing
    ( Error(..)
    , Form(..)
    , decoder
    )

import Column exposing (Column)
import Form.CompletedColumn as CompletedColumnForm
import Form.DatedColumn as DatedColumnForm
import Form.Decoder as FD
import Form.NamedTagColumn as NamedTagColumnForm
import Form.OtherTagsColumn as OtherTagsColumnForm
import Form.UndatedColumn as UndatedColumnForm
import Form.UntaggedColumn as UntaggedColumnForm



-- TYPES


type Form
    = CompletedColumnForm CompletedColumnForm.Form
    | DatedColumnForm DatedColumnForm.Form
    | NamedTagColumnForm NamedTagColumnForm.Form
    | OtherTagsColumnForm OtherTagsColumnForm.Form
    | UndatedColumnForm UndatedColumnForm.Form
    | UntaggedColumnForm UntaggedColumnForm.Form


type Error
    = CompletedColumnError CompletedColumnForm.Error
    | DatedColumnError DatedColumnForm.Error
    | NamedTagColumnError NamedTagColumnForm.Error
    | OtherTagsColumnError OtherTagsColumnForm.Error
    | UndatedColumnError UndatedColumnForm.Error
    | UntaggedColumnError UntaggedColumnForm.Error



-- DECODER


decoder : FD.Decoder Form Error Column
decoder =
    let
        subformDecoder : Form -> Result (List Error) Column
        subformDecoder form =
            case form of
                CompletedColumnForm subform ->
                    subform
                        |> FD.run CompletedColumnForm.decoder
                        |> Result.mapError (List.map CompletedColumnError)
                        |> Result.map Column.Completed

                DatedColumnForm subform ->
                    subform
                        |> FD.run DatedColumnForm.decoder
                        |> Result.mapError (List.map DatedColumnError)
                        |> Result.map Column.Dated

                NamedTagColumnForm subform ->
                    subform
                        |> FD.run NamedTagColumnForm.decoder
                        |> Result.mapError (List.map NamedTagColumnError)
                        |> Result.map Column.NamedTag

                OtherTagsColumnForm subform ->
                    subform
                        |> FD.run OtherTagsColumnForm.decoder
                        |> Result.mapError (List.map OtherTagsColumnError)
                        |> Result.map Column.OtherTags

                UndatedColumnForm subform ->
                    subform
                        |> FD.run UndatedColumnForm.decoder
                        |> Result.mapError (List.map UndatedColumnError)
                        |> Result.map Column.Undated

                UntaggedColumnForm subform ->
                    subform
                        |> FD.run UntaggedColumnForm.decoder
                        |> Result.mapError (List.map UntaggedColumnError)
                        |> Result.map Column.Untagged
    in
    FD.custom subformDecoder
