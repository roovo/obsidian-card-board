module ColumnConfig exposing
    ( ColumnConfig
    , addTaskItem
    , asColumn
    , dated
    , defaultUndated
    , defaultUntagged
    , futureColumn
    , namedTag
    , otherTags
    , todayColumn
    , tomorrowColumn
    , undated
    , untagged
    )

import Column exposing (Column, PlacementResult)
import ColumnConfig.Dated as DatedColumn exposing (DatedColumn)
import ColumnConfig.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import ColumnConfig.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import ColumnConfig.Undated as UndatedColumn exposing (UndatedColumn)
import ColumnConfig.Untagged as UntaggedColumn exposing (UntaggedColumn)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfig
    = Dated DatedColumn
    | NamedTag NamedTagColumn
    | OtherTags OtherTagsColumn
    | Undated UndatedColumn
    | Untagged UntaggedColumn



-- CONSTRUCTION


defaultUndated : ColumnConfig
defaultUndated =
    Undated <| UndatedColumn.init "Undated"


defaultUntagged : ColumnConfig
defaultUntagged =
    Untagged <| UntaggedColumn.init "Untagged"


dated : DatedColumn -> ColumnConfig
dated datedColumn =
    Dated datedColumn


futureColumn : ColumnConfig
futureColumn =
    Dated DatedColumn.future


namedTag : String -> String -> ColumnConfig
namedTag name tag =
    NamedTag <| NamedTagColumn.init name tag


otherTags : String -> List String -> ColumnConfig
otherTags name ots =
    OtherTags <| OtherTagsColumn.init name ots


todayColumn : ColumnConfig
todayColumn =
    Dated DatedColumn.forToday


tomorrowColumn : ColumnConfig
tomorrowColumn =
    Dated DatedColumn.tomorrow


undated : String -> ColumnConfig
undated name =
    Undated <| UndatedColumn.init name


untagged : String -> ColumnConfig
untagged name =
    Untagged <| UntaggedColumn.init name



-- MANIPULATION


addTaskItem : Date -> TaskItem -> ColumnConfig -> ( ColumnConfig, PlacementResult )
addTaskItem today taskItem columnConfig =
    case columnConfig of
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



-- INFO


asColumn : ColumnConfig -> Column TaskItem
asColumn columnConfig =
    case columnConfig of
        Dated datedColumn ->
            DatedColumn.asColumn datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.asColumn namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.asColumn otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.asColumn undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.asColumn untaggedColumn



-- PRIVATE


placeTask : PlacementResult -> TaskItem -> TaskList -> TaskList
placeTask pr item list =
    case pr of
        Column.CompletedInThisColumn ->
            list

        Column.DoesNotBelong ->
            list

        Column.Placed ->
            list
