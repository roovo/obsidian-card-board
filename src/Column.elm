module Column exposing
    ( Column
    , addTaskItem
    , asCompletedColumn
    , asNamedTagColumn
    , cardCount
    , cards
    , completed
    , dated
    , decoder
    , disableOthers
    , disableUndated
    , disableUntagged
    , enableOthers
    , enableUndated
    , enableUntagged
    , encoder
    , isCollapsed
    , isCompletedColumn
    , isDisabledOthers
    , isDisabledUndated
    , isDisabledUntagged
    , isEnabled
    , isEnabledOthers
    , isEnabledUndated
    , isEnabledUntagged
    , isNamedTagColumn
    , name
    , namedTag
    , otherTags
    , setCollapse
    , setTagsToHide
    , tag
    , toggleCollapse
    , undated
    , untagged
    , updateColumnNames
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
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
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


asNamedTagColumn : Column -> Maybe NamedTagColumn
asNamedTagColumn column =
    case column of
        NamedTag namedTagColumn ->
            Just namedTagColumn

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


isCompletedColumn : Column -> Bool
isCompletedColumn column =
    case column of
        Completed _ ->
            True

        _ ->
            False


isDisabledOthers : Column -> Bool
isDisabledOthers column =
    case column of
        OtherTags otherTagsColumn ->
            not <| OtherTagsColumn.isEnabled otherTagsColumn

        _ ->
            False


isDisabledUndated : Column -> Bool
isDisabledUndated column =
    case column of
        Undated undatedColumn ->
            not <| UndatedColumn.isEnabled undatedColumn

        _ ->
            False


isDisabledUntagged : Column -> Bool
isDisabledUntagged column =
    case column of
        Untagged untaggedColumn ->
            not <| UntaggedColumn.isEnabled untaggedColumn

        _ ->
            False


isEnabled : Column -> Bool
isEnabled column =
    case column of
        Completed completedColumn ->
            CompletedColumn.isEnabled completedColumn

        Dated datedColumn ->
            DatedColumn.isEnabled datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.isEnabled namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.isEnabled otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.isEnabled undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.isEnabled untaggedColumn


isEnabledOthers : Column -> Bool
isEnabledOthers column =
    case column of
        OtherTags otherTagsColumn ->
            OtherTagsColumn.isEnabled otherTagsColumn

        _ ->
            False


isEnabledUndated : Column -> Bool
isEnabledUndated column =
    case column of
        Undated undatedColumn ->
            UndatedColumn.isEnabled undatedColumn

        _ ->
            False


isEnabledUntagged : Column -> Bool
isEnabledUntagged column =
    case column of
        Untagged untaggedColumn ->
            UntaggedColumn.isEnabled untaggedColumn

        _ ->
            False


isNamedTagColumn : Column -> Bool
isNamedTagColumn column =
    case column of
        NamedTag _ ->
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


tag : Column -> Maybe String
tag column =
    case column of
        NamedTag namedTagColumn ->
            Just (NamedTagColumn.tag namedTagColumn)

        _ ->
            Nothing



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


disableOthers : Column -> Column
disableOthers column =
    case column of
        OtherTags otherTagsColumn ->
            OtherTags <| OtherTagsColumn.disable otherTagsColumn

        _ ->
            column


disableUndated : Column -> Column
disableUndated column =
    case column of
        Undated undatedColumn ->
            Undated <| UndatedColumn.disable undatedColumn

        _ ->
            column


disableUntagged : Column -> Column
disableUntagged column =
    case column of
        Untagged untaggedColumn ->
            Untagged <| UntaggedColumn.disable untaggedColumn

        _ ->
            column


enableOthers : Column -> Column
enableOthers column =
    case column of
        OtherTags otherTagsColumn ->
            OtherTags <| OtherTagsColumn.enable otherTagsColumn

        _ ->
            column


enableUndated : Column -> Column
enableUndated column =
    case column of
        Undated undatedColumn ->
            Undated <| UndatedColumn.enable undatedColumn

        _ ->
            column


enableUntagged : Column -> Column
enableUntagged column =
    case column of
        Untagged untaggedColumn ->
            Untagged <| UntaggedColumn.enable untaggedColumn

        _ ->
            column


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


updateColumnNames : DefaultColumnNames -> Column -> Column
updateColumnNames defaultColumnNames column =
    case column of
        Completed completedColumn ->
            Completed (CompletedColumn.updateName defaultColumnNames completedColumn)

        Dated datedColumn ->
            Dated (DatedColumn.updateName defaultColumnNames datedColumn)

        NamedTag namedTagColumn ->
            NamedTag (NamedTagColumn.updateName defaultColumnNames namedTagColumn)

        OtherTags otherTagsColumn ->
            OtherTags (OtherTagsColumn.updateName defaultColumnNames otherTagsColumn)

        Undated undatedColumn ->
            Undated (UndatedColumn.updateName defaultColumnNames undatedColumn)

        Untagged untaggedColumn ->
            Untagged (UntaggedColumn.updateName defaultColumnNames untaggedColumn)



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
