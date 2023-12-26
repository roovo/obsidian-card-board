module Column exposing
    ( Column(..)
    , addTaskItem
    , asCompletedColumn
    , asDatedColumn
    , asNamedTagColumn
    , asOtherTagsColumn
    , cardCount
    , cards
    , completed
    , dated
    , decoder
    , encoder
    , fromColumnConfig
    , isCollapsed
    , isCompleted
    , isDated
    , isNamedTag
    , isOtherTags
    , isUndated
    , isUntagged
    , name
    , namedTag
    , namedTagTag
    , otherTags
    , setCollapse
    , setNameToDefault
    , setTagsToHide
    , toggleCollapse
    , typeString
    , undated
    , untagged
    , updateCompletedColumnLimit
    , updateDatedColumnRangeType
    , updateDatedColumnRangeValueFrom
    , updateDatedColumnRangeValueTo
    , updateName
    , updateNamedTagTag
    , updateOtherTags
    )

import Card exposing (Card)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.Dated as DatedColumn exposing (DatedColumn)
import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import Column.Undated as UndatedColumn exposing (UndatedColumn)
import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
import Date exposing (Date)
import DecodeHelpers
import DefaultColumnNames exposing (DefaultColumnNames)
import Form.NewColumnConfig exposing (NewColumnConfigForm)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TsJson.Decode as TsDecode
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


fromColumnConfig : DefaultColumnNames -> NewColumnConfigForm -> Maybe Column
fromColumnConfig defaultColumnNames newColumnConfigForm =
    let
        newName : String
        newName =
            if String.isEmpty (String.trim newColumnConfigForm.name) then
                DefaultColumnNames.nameFor newColumnConfigForm.columnType defaultColumnNames

            else
                newColumnConfigForm.name
    in
    case newColumnConfigForm.columnType of
        "completed" ->
            Just (Completed <| CompletedColumn.init newName 0 10)

        "dated" ->
            let
                todayName : String
                todayName =
                    if String.isEmpty (String.trim newColumnConfigForm.name) then
                        DefaultColumnNames.nameFor "today" defaultColumnNames

                    else
                        newColumnConfigForm.name
            in
            Just (Dated <| DatedColumn.init todayName (DatedColumn.Before 1))

        "namedTag" ->
            Just (namedTag newColumnConfigForm.name "")

        "otherTags" ->
            Just (otherTags newName [])

        "undated" ->
            Just (undated newName)

        "untagged" ->
            Just (untagged newName)

        _ ->
            Nothing


namedTag : String -> String -> Column
namedTag name_ tag_ =
    NamedTag <| NamedTagColumn.init name_ tag_


otherTags : String -> List String -> Column
otherTags name_ ots =
    OtherTags <| OtherTagsColumn.init name_ ots


undated : String -> Column
undated name_ =
    Undated <| UndatedColumn.init name_


untagged : String -> Column
untagged name_ =
    Untagged <| UntaggedColumn.init name_



-- DECODE / ENCODE


decoder : TsDecode.Decoder Column
decoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "completed" Completed CompletedColumn.decoder
        , DecodeHelpers.toElmVariant "dated" Dated DatedColumn.decoder
        , DecodeHelpers.toElmVariant "namedTag" NamedTag NamedTagColumn.decoder
        , DecodeHelpers.toElmVariant "otherTags" OtherTags OtherTagsColumn.decoder
        , DecodeHelpers.toElmVariant "undated" Undated UndatedColumn.decoder
        , DecodeHelpers.toElmVariant "untagged" Untagged UntaggedColumn.decoder
        ]


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
        |> TsEncode.variantTagged "undated" UndatedColumn.encoder
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


asDatedColumn : Column -> Maybe DatedColumn
asDatedColumn column =
    case column of
        Dated datedColumn ->
            Just datedColumn

        _ ->
            Nothing


asNamedTagColumn : Column -> Maybe NamedTagColumn
asNamedTagColumn column =
    case column of
        NamedTag namedTagColumn ->
            Just namedTagColumn

        _ ->
            Nothing


asOtherTagsColumn : Column -> Maybe OtherTagsColumn
asOtherTagsColumn column =
    case column of
        OtherTags otherTagsColumn ->
            Just otherTagsColumn

        _ ->
            Nothing


cardCount : Column -> Int
cardCount column =
    List.length (toList column)


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


isCompleted : Column -> Bool
isCompleted column =
    case column of
        Completed _ ->
            True

        _ ->
            False


isDated : Column -> Bool
isDated column =
    case column of
        Dated _ ->
            True

        _ ->
            False


isNamedTag : Column -> Bool
isNamedTag column =
    case column of
        NamedTag _ ->
            True

        _ ->
            False


isOtherTags : Column -> Bool
isOtherTags column =
    case column of
        OtherTags _ ->
            True

        _ ->
            False


isUndated : Column -> Bool
isUndated column =
    case column of
        Undated _ ->
            True

        _ ->
            False


isUntagged : Column -> Bool
isUntagged column =
    case column of
        Untagged _ ->
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


namedTagTag : Column -> Maybe String
namedTagTag column =
    case column of
        NamedTag namedTagColumn ->
            Just (NamedTagColumn.tag namedTagColumn)

        _ ->
            Nothing


typeString : Column -> String
typeString column =
    case column of
        Completed _ ->
            "Completed"

        Dated _ ->
            "Dated"

        NamedTag _ ->
            "Tagged"

        OtherTags _ ->
            "Other Tags"

        Undated _ ->
            "Undated"

        Untagged _ ->
            "Untagged"



-- MANIPULATION


addTaskItem : Date -> TaskItem -> Column -> ( Column, PlacementResult )
addTaskItem today taskItem column =
    case column of
        Completed _ ->
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


setCollapse : Bool -> Column -> Column
setCollapse isCollapsed_ column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.setCollapse isCollapsed_ completedColumn)

        Dated datedColumn ->
            Dated (DatedColumn.setCollapse isCollapsed_ datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.setCollapse isCollapsed_ namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.setCollapse isCollapsed_ otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.setCollapse isCollapsed_ undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.setCollapse isCollapsed_ untaggedColumn)


setNameToDefault : DefaultColumnNames -> Column -> Column
setNameToDefault defaultColumnNames column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.setNameToDefault defaultColumnNames completedColumn)

        Dated datedColumn ->
            Dated (DatedColumn.setNameToDefault defaultColumnNames datedColumn)

        NamedTag _ ->
            column

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.setNameToDefault defaultColumnNames otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.setNameToDefault defaultColumnNames undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.setNameToDefault defaultColumnNames untaggedColumn)


setTagsToHide : List String -> Column -> Column
setTagsToHide tags column =
    case column of
        Completed completedColumn ->
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


toggleCollapse : Column -> Column
toggleCollapse column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.toggleCollapse completedColumn)

        Dated datedColumn ->
            Dated (DatedColumn.toggleCollapse datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.toggleCollapse namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.toggleCollapse otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.toggleCollapse undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.toggleCollapse untaggedColumn)


updateCompletedColumnLimit : Int -> Column -> Column
updateCompletedColumnLimit newLimit column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.updateCompletedCount newLimit completedColumn)

        _ ->
            column


updateDatedColumnRangeType : String -> Column -> Column
updateDatedColumnRangeType rangeType column =
    case column of
        Dated datedColumn ->
            Dated (DatedColumn.updateRangeType rangeType datedColumn)

        _ ->
            column


updateDatedColumnRangeValueFrom : Int -> Column -> Column
updateDatedColumnRangeValueFrom newValue column =
    case column of
        Dated datedColumn ->
            Dated (DatedColumn.updateRangeValueFrom newValue datedColumn)

        _ ->
            column


updateDatedColumnRangeValueTo : Int -> Column -> Column
updateDatedColumnRangeValueTo newValue column =
    case column of
        Dated datedColumn ->
            Dated (DatedColumn.updateRangeValueTo newValue datedColumn)

        _ ->
            column


updateName : String -> Column -> Column
updateName newName column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.updateName newName completedColumn)

        Dated datedColumn ->
            Dated (DatedColumn.updateName newName datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.updateName newName namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.updateName newName otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.updateName newName undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.updateName newName untaggedColumn)


updateNamedTagTag : String -> Column -> Column
updateNamedTagTag newTag column =
    case column of
        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.updateTag newTag namedTagColumn)

        _ ->
            column


updateOtherTags : (OtherTagsColumn -> OtherTagsColumn) -> Column -> Column
updateOtherTags fn column =
    case column of
        OtherTags otherTagsColumn ->
            OtherTags <| fn otherTagsColumn

        _ ->
            column



-- PRIVATE


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
