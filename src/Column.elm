module Column exposing
    ( Column
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
completed completedColumn =
    Completed completedColumn


defaultUndated : Column
defaultUndated =
    Undated <| UndatedColumn.init "Undated"


defaultUntagged : Column
defaultUntagged =
    Untagged <| UntaggedColumn.init "Untagged"


dated : DatedColumn -> Column
dated datedColumn =
    Dated datedColumn


futureColumn : Column
futureColumn =
    Dated DatedColumn.future


namedTag : String -> String -> Column
namedTag name_ tag =
    NamedTag <| NamedTagColumn.init name_ tag


otherTags : String -> List String -> Column
otherTags name_ ots =
    OtherTags <| OtherTagsColumn.init name_ ots


todayColumn : Column
todayColumn =
    Dated DatedColumn.forToday


tomorrowColumn : Column
tomorrowColumn =
    Dated DatedColumn.tomorrow


undated : String -> Column
undated name_ =
    Undated <| UndatedColumn.init name_


untagged : String -> Column
untagged name_ =
    Untagged <| UntaggedColumn.init name_



-- INFO


cards : String -> Column -> List Card
cards boardId column =
    let
        cardIdPrefix : String
        cardIdPrefix =
            boardId ++ ":" ++ name column
    in
    taskList column
        |> TaskList.topLevelTasks
        |> List.map (Card.fromTaskItem cardIdPrefix [])


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


taskList : Column -> TaskList
taskList column =
    case column of
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
