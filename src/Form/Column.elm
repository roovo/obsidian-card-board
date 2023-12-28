module Form.Column exposing
    ( ColumnForm(..)
    , Error(..)
    , init
    , isCompleted
    , isOtherTags
    , isUndated
    , isUntagged
    , name
    , placeholder
    , safeDecoder
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
import Form.Column.Completed as CompletedColumnForm exposing (CompletedColumnForm)
import Form.Column.Dated as DatedColumnForm exposing (DatedColumnForm)
import Form.Column.NamedTag as NamedTagColumnForm exposing (NamedTagColumnForm)
import Form.Column.OtherTags as OtherTagsColumnForm exposing (OtherTagsColumnForm)
import Form.Column.Undated as UndatedColumnForm exposing (UndatedColumnForm)
import Form.Column.Untagged as UntaggedColumnForm exposing (UntaggedColumnForm)
import Form.SafeDecoder as SD



-- TYPES


type ColumnForm
    = CompletedColumnForm CompletedColumnForm
    | DatedColumnForm DatedColumnForm
    | NamedTagColumnForm NamedTagColumnForm
    | OtherTagsColumnForm OtherTagsColumnForm
    | UndatedColumnForm UndatedColumnForm
    | UntaggedColumnForm UntaggedColumnForm


type Error
    = CompletedColumnError CompletedColumnForm.Error
    | DatedColumnError DatedColumnForm.Error
    | NamedTagColumnError NamedTagColumnForm.Error
    | OtherTagsColumnError OtherTagsColumnForm.Error
    | UndatedColumnError UndatedColumnForm.Error
    | UntaggedColumnError UntaggedColumnForm.Error



-- CONSTRUCTION


init : Column -> ColumnForm
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


safeDecoder : SD.Decoder ColumnForm Column
safeDecoder =
    let
        subformDecoder : ColumnForm -> Result Never Column
        subformDecoder form =
            case form of
                CompletedColumnForm subform ->
                    subform
                        |> SD.run CompletedColumnForm.safeDecoder
                        |> Result.map Column.Completed

                DatedColumnForm subform ->
                    subform
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Result.map Column.Dated

                NamedTagColumnForm subform ->
                    subform
                        |> SD.run NamedTagColumnForm.safeDecoder
                        |> Result.map Column.NamedTag

                OtherTagsColumnForm subform ->
                    subform
                        |> SD.run OtherTagsColumnForm.safeDecoder
                        |> Result.map Column.OtherTags

                UndatedColumnForm subform ->
                    subform
                        |> SD.run UndatedColumnForm.safeDecoder
                        |> Result.map Column.Undated

                UntaggedColumnForm subform ->
                    subform
                        |> SD.run UntaggedColumnForm.safeDecoder
                        |> Result.map Column.Untagged
    in
    SD.custom subformDecoder



-- INFO


isCompleted : ColumnForm -> Bool
isCompleted form =
    case form of
        CompletedColumnForm _ ->
            True

        _ ->
            False


isOtherTags : ColumnForm -> Bool
isOtherTags form =
    case form of
        OtherTagsColumnForm _ ->
            True

        _ ->
            False


isUndated : ColumnForm -> Bool
isUndated form =
    case form of
        UndatedColumnForm _ ->
            True

        _ ->
            False


isUntagged : ColumnForm -> Bool
isUntagged form =
    case form of
        UntaggedColumnForm _ ->
            True

        _ ->
            False


name : ColumnForm -> String
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


placeholder : DefaultColumnNames -> ColumnForm -> String
placeholder defaultColumnNames form =
    case form of
        CompletedColumnForm _ ->
            DefaultColumnNames.nameFor "completed" defaultColumnNames

        DatedColumnForm _ ->
            ""

        NamedTagColumnForm _ ->
            ""

        OtherTagsColumnForm _ ->
            DefaultColumnNames.nameFor "otherTags" defaultColumnNames

        UndatedColumnForm _ ->
            DefaultColumnNames.nameFor "undated" defaultColumnNames

        UntaggedColumnForm _ ->
            DefaultColumnNames.nameFor "untagged" defaultColumnNames


typeString : ColumnForm -> String
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


updateCompletedColumnLimit : String -> ColumnForm -> ColumnForm
updateCompletedColumnLimit newLimit form =
    case form of
        CompletedColumnForm subform ->
            CompletedColumnForm <| CompletedColumnForm.updateLimit newLimit subform

        _ ->
            form


updateDatedColumnRangeType : String -> ColumnForm -> ColumnForm
updateDatedColumnRangeType newType form =
    case form of
        DatedColumnForm subform ->
            DatedColumnForm <| DatedColumnForm.updateRangeType newType subform

        _ ->
            form


updateDatedColumnRangeValueFrom : String -> ColumnForm -> ColumnForm
updateDatedColumnRangeValueFrom newValue form =
    case form of
        DatedColumnForm subform ->
            DatedColumnForm <| DatedColumnForm.updateFrom newValue subform

        _ ->
            form


updateDatedColumnRangeValueTo : String -> ColumnForm -> ColumnForm
updateDatedColumnRangeValueTo newValue form =
    case form of
        DatedColumnForm subform ->
            DatedColumnForm <| DatedColumnForm.updateTo newValue subform

        _ ->
            form


updateName : String -> ColumnForm -> ColumnForm
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


updateNamedTagTag : String -> ColumnForm -> ColumnForm
updateNamedTagTag newName form =
    case form of
        NamedTagColumnForm subform ->
            NamedTagColumnForm <| NamedTagColumnForm.updateTag newName subform

        _ ->
            form
