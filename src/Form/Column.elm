module Form.Column exposing
    ( ColumnForm(..)
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
    = CompletedColumnForm Bool CompletedColumnForm
    | DatedColumnForm Bool DatedColumnForm
    | NamedTagColumnForm Bool NamedTagColumnForm
    | OtherTagsColumnForm Bool OtherTagsColumnForm
    | UndatedColumnForm Bool UndatedColumnForm
    | UntaggedColumnForm Bool UntaggedColumnForm



-- CONSTRUCTION


init : Column -> ColumnForm
init column =
    case column of
        Column.Completed completedColumn ->
            CompletedColumnForm (Column.isCollapsed column) <| CompletedColumnForm.init completedColumn

        Column.Dated datedColumn ->
            DatedColumnForm (Column.isCollapsed column) <| DatedColumnForm.init datedColumn

        Column.NamedTag namedTagColumn ->
            NamedTagColumnForm (Column.isCollapsed column) <| NamedTagColumnForm.init namedTagColumn

        Column.OtherTags otherTagsColumn ->
            OtherTagsColumnForm (Column.isCollapsed column) <| OtherTagsColumnForm.init otherTagsColumn

        Column.Undated undatedColumn ->
            UndatedColumnForm (Column.isCollapsed column) <| UndatedColumnForm.init undatedColumn

        Column.Untagged untaggedColumn ->
            UntaggedColumnForm (Column.isCollapsed column) <| UntaggedColumnForm.init untaggedColumn



-- DECODER


safeDecoder : SD.Decoder ColumnForm Column
safeDecoder =
    let
        subformDecoder : ColumnForm -> Result Never Column
        subformDecoder form =
            case form of
                CompletedColumnForm isCollapsed_ subform ->
                    subform
                        |> SD.run CompletedColumnForm.safeDecoder
                        |> Result.map Column.Completed
                        |> Result.map (Column.setCollapse isCollapsed_)

                DatedColumnForm isCollapsed_ subform ->
                    subform
                        |> SD.run DatedColumnForm.safeDecoder
                        |> Result.map Column.Dated
                        |> Result.map (Column.setCollapse isCollapsed_)

                NamedTagColumnForm isCollapsed_ subform ->
                    subform
                        |> SD.run NamedTagColumnForm.safeDecoder
                        |> Result.map Column.NamedTag
                        |> Result.map (Column.setCollapse isCollapsed_)

                OtherTagsColumnForm isCollapsed_ subform ->
                    subform
                        |> SD.run OtherTagsColumnForm.safeDecoder
                        |> Result.map Column.OtherTags
                        |> Result.map (Column.setCollapse isCollapsed_)

                UndatedColumnForm isCollapsed_ subform ->
                    subform
                        |> SD.run UndatedColumnForm.safeDecoder
                        |> Result.map Column.Undated
                        |> Result.map (Column.setCollapse isCollapsed_)

                UntaggedColumnForm isCollapsed_ subform ->
                    subform
                        |> SD.run UntaggedColumnForm.safeDecoder
                        |> Result.map Column.Untagged
                        |> Result.map (Column.setCollapse isCollapsed_)
    in
    SD.custom subformDecoder



-- INFO


isCompleted : ColumnForm -> Bool
isCompleted form =
    case form of
        CompletedColumnForm _ _ ->
            True

        _ ->
            False


isOtherTags : ColumnForm -> Bool
isOtherTags form =
    case form of
        OtherTagsColumnForm _ _ ->
            True

        _ ->
            False


isUndated : ColumnForm -> Bool
isUndated form =
    case form of
        UndatedColumnForm _ _ ->
            True

        _ ->
            False


isUntagged : ColumnForm -> Bool
isUntagged form =
    case form of
        UntaggedColumnForm _ _ ->
            True

        _ ->
            False


name : ColumnForm -> String
name form =
    case form of
        CompletedColumnForm _ subform ->
            subform.name

        DatedColumnForm _ subform ->
            subform.name

        NamedTagColumnForm _ subform ->
            subform.name

        OtherTagsColumnForm _ subform ->
            subform.name

        UndatedColumnForm _ subform ->
            subform.name

        UntaggedColumnForm _ subform ->
            subform.name


placeholder : DefaultColumnNames -> ColumnForm -> String
placeholder defaultColumnNames form =
    case form of
        CompletedColumnForm _ _ ->
            DefaultColumnNames.nameFor "completed" defaultColumnNames

        DatedColumnForm _ _ ->
            ""

        NamedTagColumnForm _ _ ->
            ""

        OtherTagsColumnForm _ _ ->
            DefaultColumnNames.nameFor "otherTags" defaultColumnNames

        UndatedColumnForm _ _ ->
            DefaultColumnNames.nameFor "undated" defaultColumnNames

        UntaggedColumnForm _ _ ->
            DefaultColumnNames.nameFor "untagged" defaultColumnNames


typeString : ColumnForm -> String
typeString form =
    case form of
        CompletedColumnForm _ _ ->
            "Completed"

        DatedColumnForm _ _ ->
            "Dated"

        NamedTagColumnForm _ _ ->
            "Tagged"

        OtherTagsColumnForm _ _ ->
            "Other Tags"

        UndatedColumnForm _ _ ->
            "Undated"

        UntaggedColumnForm _ _ ->
            "Untagged"



-- MODIFICATION


updateCompletedColumnLimit : String -> ColumnForm -> ColumnForm
updateCompletedColumnLimit newLimit form =
    case form of
        CompletedColumnForm isCollapsed_ subform ->
            CompletedColumnForm isCollapsed_ <| CompletedColumnForm.updateLimit newLimit subform

        _ ->
            form


updateDatedColumnRangeType : String -> ColumnForm -> ColumnForm
updateDatedColumnRangeType newType form =
    case form of
        DatedColumnForm isCollapsed_ subform ->
            DatedColumnForm isCollapsed_ <| DatedColumnForm.updateRangeType newType subform

        _ ->
            form


updateDatedColumnRangeValueFrom : String -> ColumnForm -> ColumnForm
updateDatedColumnRangeValueFrom newValue form =
    case form of
        DatedColumnForm isCollapsed_ subform ->
            DatedColumnForm isCollapsed_ <| DatedColumnForm.updateFrom newValue subform

        _ ->
            form


updateDatedColumnRangeValueTo : String -> ColumnForm -> ColumnForm
updateDatedColumnRangeValueTo newValue form =
    case form of
        DatedColumnForm isCollapsed_ subform ->
            DatedColumnForm isCollapsed_ <| DatedColumnForm.updateTo newValue subform

        _ ->
            form


updateName : String -> ColumnForm -> ColumnForm
updateName newName form =
    case form of
        CompletedColumnForm isCollapsed_ subform ->
            CompletedColumnForm isCollapsed_ <| CompletedColumnForm.updateName newName subform

        DatedColumnForm isCollapsed_ subform ->
            DatedColumnForm isCollapsed_ <| DatedColumnForm.updateName newName subform

        NamedTagColumnForm isCollapsed_ subform ->
            NamedTagColumnForm isCollapsed_ <| NamedTagColumnForm.updateName newName subform

        OtherTagsColumnForm isCollapsed_ subform ->
            OtherTagsColumnForm isCollapsed_ <| OtherTagsColumnForm.updateName newName subform

        UndatedColumnForm isCollapsed_ subform ->
            UndatedColumnForm isCollapsed_ <| UndatedColumnForm.updateName newName subform

        UntaggedColumnForm isCollapsed_ subform ->
            UntaggedColumnForm isCollapsed_ <| UntaggedColumnForm.updateName newName subform


updateNamedTagTag : String -> ColumnForm -> ColumnForm
updateNamedTagTag newName form =
    case form of
        NamedTagColumnForm isCollapsed_ subform ->
            NamedTagColumnForm isCollapsed_ <| NamedTagColumnForm.updateTag newName subform

        _ ->
            form



-- PRIVATE
