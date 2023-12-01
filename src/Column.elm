module Column exposing
    ( Column
    , addTaskItem
    , asCompletedColumn
    , cards
    , completed
    , dated
    , defaultUntagged
    , encoder
    , isCollapsed
    , isCompletedColumn
    , name
    , namedTag
    , otherTags
    , setTagsToHide
    , undated
    , untagged
    )

import Card exposing (Card)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.Dated as DatedColumn exposing (DatedColumn)
import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Column.Undated as UndatedColumn exposing (UndatedColumn)
import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Encode as TsEncode



-- TYPES


type Column
    = Completed CompletedColumn
    | Dated DatedColumn
    | NamedTag NamedTagColumn
    | OtherTags OtherTagsColumn
    | Undated UndatedColumn
    | Untagged UntaggedColumn



-- CONSTRUCTION


completed : CompletedColumn -> Column
completed =
    Completed


dated : DatedColumn -> Column
dated =
    Dated


defaultUntagged : Column
defaultUntagged =
    Untagged <| UntaggedColumn.init "Untagged"


namedTag : String -> String -> Column
namedTag name_ tag =
    NamedTag <| NamedTagColumn.init name_ tag


otherTags : String -> List String -> Column
otherTags name_ ots =
    OtherTags <| OtherTagsColumn.init name_ ots


undated : String -> Column
undated name_ =
    Undated <| UndatedColumn.init name_


untagged : String -> Column
untagged name_ =
    Untagged <| UntaggedColumn.init name_



-- ENCODE


encoder : TsEncode.Encoder Column
encoder =
    TsEncode.union
        (\vCompleted vDated vNamedTag vOtherTags vUndated vUntagged value ->
            case value of
                Completed config ->
                    vCompleted config

                Dated config ->
                    vDated config

                NamedTag config ->
                    vNamedTag config

                OtherTags config ->
                    vOtherTags config

                Undated config ->
                    vUndated config

                Untagged config ->
                    vUntagged config
        )
        |> TsEncode.variantTagged "completed" CompletedColumn.encoder
        |> TsEncode.variantTagged "dated" DatedColumn.encoder
        |> TsEncode.variantTagged "namedTag" NamedTagColumn.encoder
        |> TsEncode.variantTagged "otherTags" OtherTagsColumn.encoder
        |> TsEncode.variantTagged "undatged" UndatedColumn.encoder
        |> TsEncode.variantTagged "untagged" UntaggedColumn.encoder
        |> TsEncode.buildUnion



-- INFO


asCompletedColumn : Column -> Maybe CompletedColumn
asCompletedColumn column =
    case column of
        Completed completedColumn ->
            Just completedColumn

        _ ->
            Nothing


cards : String -> Column -> List Card
cards boardId column =
    let
        cardIdPrefix : String
        cardIdPrefix =
            boardId ++ ":" ++ name column
    in
    toList column
        |> List.map (Card.fromTaskItem cardIdPrefix (tagsToHide column))


isCollapsed : Column -> Bool
isCollapsed column =
    case column of
        Completed completedColumn ->
            CompletedColumn.isCollapsed completedColumn

        Dated datedColumn ->
            DatedColumn.isCollapsed datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.isCollapsed namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.isCollapsed otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.isCollapsed undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.isCollapsed untaggedColumn


isCompletedColumn : Column -> Bool
isCompletedColumn column =
    case column of
        Completed _ ->
            True

        _ ->
            False


name : Column -> String
name column =
    case column of
        Completed completedColumn ->
            CompletedColumn.name completedColumn

        Dated datedColumn ->
            DatedColumn.name datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.name namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.name otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.name undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.name untaggedColumn



-- MANIPULATION


addTaskItem : Date -> TaskItem -> Column -> ( Column, PlacementResult )
addTaskItem today taskItem column =
    case column of
        Completed completedColumn ->
            -- TODO: This shouldn't be here !! just hacking for now
            ( column, PlacementResult.DoesNotBelong )

        Dated datedColumn ->
            DatedColumn.addTaskItem today taskItem datedColumn
                |> Tuple.mapFirst Dated

        NamedTag namedTagColumn ->
            NamedTagColumn.addTaskItem taskItem namedTagColumn
                |> Tuple.mapFirst NamedTag

        OtherTags otherTagsColumn ->
            OtherTagsColumn.addTaskItem taskItem otherTagsColumn
                |> Tuple.mapFirst OtherTags

        Undated undatedColumn ->
            UndatedColumn.addTaskItem taskItem undatedColumn
                |> Tuple.mapFirst Undated

        Untagged untaggedColumn ->
            UntaggedColumn.addTaskItem taskItem untaggedColumn
                |> Tuple.mapFirst Untagged


setTagsToHide : List String -> Column -> Column
setTagsToHide tags column =
    case column of
        Completed completedColumn ->
            -- TODO: This shouldn't be here !! just hacking for now
            Completed (CompletedColumn.setTagsToHide tags completedColumn)

        Dated datedColumn ->
            Dated (DatedColumn.setTagsToHide tags datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.setTagsToHide tags namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.setTagsToHide tags otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.setTagsToHide tags undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.setTagsToHide tags untaggedColumn)



-- PRIVATE


placeTask : PlacementResult -> TaskItem -> TaskList -> TaskList
placeTask pr item list =
    case pr of
        PlacementResult.CompletedInThisColumn ->
            list

        PlacementResult.DoesNotBelong ->
            list

        PlacementResult.Placed ->
            list


tagsToHide : Column -> List String
tagsToHide column =
    case column of
        Completed completedColumn ->
            CompletedColumn.tagsToHide completedColumn

        Dated datedColumn ->
            DatedColumn.tagsToHide datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.tagsToHide namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.tagsToHide otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.tagsToHide undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.tagsToHide untaggedColumn


toList : Column -> List TaskItem
toList column =
    case column of
        Completed completedColumn ->
            CompletedColumn.toList completedColumn

        Dated datedColumn ->
            DatedColumn.toList datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.toList namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.toList otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.toList undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.toList untaggedColumn
