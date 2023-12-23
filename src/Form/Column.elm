module Form.Column exposing
    ( Error(..)
    , Form(..)
    , decoder
    , fromColumnConfig
    , init
    , isCompleted
    , isOtherTags
    , isUndated
    , isUntagged
    , name
    , typeString
    , updateCompletedColumnLimit
    , updateDatedColumnRangeType
    , updateDatedColumnRangeValueFrom
    , updateDatedColumnRangeValueTo
    , updateName
    , updateNamedTagTag
    )

import Column exposing (Column)
import DefaultColumnNames exposing (DefaultColumnNames)
import Form.CompletedColumn as CompletedColumnForm
import Form.DatedColumn as DatedColumnForm
import Form.Decoder as FD
import Form.NamedTagColumn as NamedTagColumnForm
import Form.OtherTagsColumn as OtherTagsColumnForm
import Form.UndatedColumn as UndatedColumnForm
import Form.UntaggedColumn as UntaggedColumnForm
import NewColumnConfig exposing (NewColumnConfig)



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



-- CONSTRUCTION


fromColumnConfig : DefaultColumnNames -> NewColumnConfig -> Maybe Form
fromColumnConfig defaultColumnNames newColumnConfig =
    let
        newName : String
        newName =
            if String.isEmpty (String.trim newColumnConfig.name) then
                DefaultColumnNames.nameFor newColumnConfig.columnType defaultColumnNames

            else
                newColumnConfig.name
    in
    case newColumnConfig.columnType of
        "completed" ->
            Just (CompletedColumnForm { name = newName, limit = "10" })

        "dated" ->
            Just (DatedColumnForm { name = newColumnConfig.name, rangeType = "Before", from = "", to = "1" })

        "namedTag" ->
            Just (NamedTagColumnForm { name = newColumnConfig.name, tag = "" })

        "otherTags" ->
            Just (OtherTagsColumnForm { name = newName })

        "undated" ->
            Just (UndatedColumnForm { name = newName })

        "untagged" ->
            Just (UntaggedColumnForm { name = newName })

        _ ->
            Nothing


init : Column -> Form
init column =
    case column of
        Column.Completed completedColumn ->
            CompletedColumnForm <| CompletedColumnForm.init completedColumn

        Column.Dated datedColumn ->
            DatedColumnForm <| DatedColumnForm.init datedColumn

        Column.NamedTag namedTagColumn ->
            NamedTagColumnForm <| NamedTagColumnForm.init namedTagColumn

        Column.OtherTags otherTagsColumn ->
            OtherTagsColumnForm <| OtherTagsColumnForm.init otherTagsColumn

        Column.Undated undatedColumn ->
            UndatedColumnForm <| UndatedColumnForm.init undatedColumn

        Column.Untagged untaggedColumn ->
            UntaggedColumnForm <| UntaggedColumnForm.init untaggedColumn



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



-- INFO


isCompleted : Form -> Bool
isCompleted form =
    case form of
        CompletedColumnForm _ ->
            True

        _ ->
            False


isOtherTags : Form -> Bool
isOtherTags form =
    case form of
        OtherTagsColumnForm _ ->
            True

        _ ->
            False


isUndated : Form -> Bool
isUndated form =
    case form of
        UndatedColumnForm _ ->
            True

        _ ->
            False


isUntagged : Form -> Bool
isUntagged form =
    case form of
        UntaggedColumnForm _ ->
            True

        _ ->
            False


name : Form -> String
name form =
    case form of
        CompletedColumnForm subform ->
            subform.name

        DatedColumnForm subform ->
            subform.name

        NamedTagColumnForm subform ->
            subform.name

        OtherTagsColumnForm subform ->
            subform.name

        UndatedColumnForm subform ->
            subform.name

        UntaggedColumnForm subform ->
            subform.name


typeString : Form -> String
typeString form =
    case form of
        CompletedColumnForm subform ->
            "Completed"

        DatedColumnForm subform ->
            "Dated"

        NamedTagColumnForm subform ->
            "Tagged"

        OtherTagsColumnForm subform ->
            "Other Tags"

        UndatedColumnForm subform ->
            "Undated"

        UntaggedColumnForm subform ->
            "Untagged"



-- MODIFICATION


updateCompletedColumnLimit : String -> Form -> Form
updateCompletedColumnLimit newLimit form =
    case form of
        CompletedColumnForm subform ->
            CompletedColumnForm <| CompletedColumnForm.updateLimit newLimit subform

        _ ->
            form


updateDatedColumnRangeType : String -> Form -> Form
updateDatedColumnRangeType newType form =
    case form of
        DatedColumnForm subform ->
            DatedColumnForm <| DatedColumnForm.updateRangeType newType subform

        _ ->
            form


updateDatedColumnRangeValueFrom : String -> Form -> Form
updateDatedColumnRangeValueFrom newValue form =
    case form of
        DatedColumnForm subform ->
            DatedColumnForm <| DatedColumnForm.updateFrom newValue subform

        _ ->
            form


updateDatedColumnRangeValueTo : String -> Form -> Form
updateDatedColumnRangeValueTo newValue form =
    case form of
        DatedColumnForm subform ->
            DatedColumnForm <| DatedColumnForm.updateTo newValue subform

        _ ->
            form


updateName : String -> Form -> Form
updateName newName form =
    case form of
        CompletedColumnForm subform ->
            CompletedColumnForm <| CompletedColumnForm.updateName newName subform

        DatedColumnForm subform ->
            DatedColumnForm <| DatedColumnForm.updateName newName subform

        NamedTagColumnForm subform ->
            NamedTagColumnForm <| NamedTagColumnForm.updateName newName subform

        OtherTagsColumnForm subform ->
            OtherTagsColumnForm <| OtherTagsColumnForm.updateName newName subform

        UndatedColumnForm subform ->
            UndatedColumnForm <| UndatedColumnForm.updateName newName subform

        UntaggedColumnForm subform ->
            UntaggedColumnForm <| UntaggedColumnForm.updateName newName subform


updateNamedTagTag : String -> Form -> Form
updateNamedTagTag newName form =
    case form of
        NamedTagColumnForm subform ->
            NamedTagColumnForm <| NamedTagColumnForm.updateTag newName subform

        _ ->
            form
