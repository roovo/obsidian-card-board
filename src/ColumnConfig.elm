module ColumnConfig exposing
    ( ColumnConfig
    , addTaskItem
    , cards
    , completed
    , dated
    , defaultUndated
    , defaultUntagged
    , futureColumn
    , isCollapsed
    , name
    , namedTag
    , otherTags
    , todayColumn
    , tomorrowColumn
    , undated
    , untagged
    )

import Card exposing (Card)
import ColumnConfig.Completed as CompletedColumn exposing (CompletedColumn)
import ColumnConfig.Dated as DatedColumn exposing (DatedColumn)
import ColumnConfig.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import ColumnConfig.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import ColumnConfig.Undated as UndatedColumn exposing (UndatedColumn)
import ColumnConfig.Untagged as UntaggedColumn exposing (UntaggedColumn)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type ColumnConfig
    = Completed CompletedColumn
    | Dated DatedColumn
    | NamedTag NamedTagColumn
    | OtherTags OtherTagsColumn
    | Undated UndatedColumn
    | Untagged UntaggedColumn



-- CONSTRUCTION


completed : CompletedColumn -> ColumnConfig
completed completedColumn =
    Completed completedColumn


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
namedTag name_ tag =
    NamedTag <| NamedTagColumn.init name_ tag


otherTags : String -> List String -> ColumnConfig
otherTags name_ ots =
    OtherTags <| OtherTagsColumn.init name_ ots


todayColumn : ColumnConfig
todayColumn =
    Dated DatedColumn.forToday


tomorrowColumn : ColumnConfig
tomorrowColumn =
    Dated DatedColumn.tomorrow


undated : String -> ColumnConfig
undated name_ =
    Undated <| UndatedColumn.init name_


untagged : String -> ColumnConfig
untagged name_ =
    Untagged <| UntaggedColumn.init name_



-- INFO


cards : ColumnConfig -> List Card
cards columnConfig =
    taskList columnConfig
        |> TaskList.topLevelTasks
        |> List.map (Card.fromTaskItem "" [])


isCollapsed : ColumnConfig -> Bool
isCollapsed columnConfig =
    case columnConfig of
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


name : ColumnConfig -> String
name columnConfig =
    case columnConfig of
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


addTaskItem : Date -> TaskItem -> ColumnConfig -> ( ColumnConfig, PlacementResult )
addTaskItem today taskItem columnConfig =
    case columnConfig of
        Completed completedColumn ->
            -- TODO: This shouldn't be here !! just hacking for now
            ( columnConfig, PlacementResult.DoesNotBelong )

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


taskList : ColumnConfig -> TaskList
taskList columnConfig =
    case columnConfig of
        Completed completedColumn ->
            CompletedColumn.taskList completedColumn

        Dated datedColumn ->
            DatedColumn.taskList datedColumn

        NamedTag namedTagColumn ->
            NamedTagColumn.taskList namedTagColumn

        OtherTags otherTagsColumn ->
            OtherTagsColumn.taskList otherTagsColumn

        Undated undatedColumn ->
            UndatedColumn.taskList undatedColumn

        Untagged untaggedColumn ->
            UntaggedColumn.taskList untaggedColumn
