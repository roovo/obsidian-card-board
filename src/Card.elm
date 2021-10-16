module Card exposing
    ( Card
    , Highlight(..)
    , fromTaskItem
    , highlight
    , id
    , subtasks
    , taskItem
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import Time



-- TYPES


type Card
    = Card String TaskItem


type Highlight
    = HighlightNone
    | HighlightCritical
    | HighlightGood
    | HighlightImportant



-- CONSTRUCTION


fromTaskItem : Int -> String -> TaskItem -> Card
fromTaskItem panelIndex columnId item =
    Card (String.fromInt panelIndex ++ ":" ++ columnId ++ ":") item



-- INFO


highlight : Time.Posix -> Time.Zone -> Card -> Highlight
highlight now zone (Card _ item) =
    case ( TaskItem.isCompleted item, TaskItem.due item ) of
        ( False, Just dueDate ) ->
            if Date.fromPosix zone now == dueDate then
                HighlightImportant

            else if Date.toRataDie (Date.fromPosix zone now) > Date.toRataDie dueDate then
                HighlightCritical

            else if Date.toRataDie (Date.fromPosix zone now) < Date.toRataDie dueDate then
                HighlightGood

            else
                HighlightNone

        ( _, _ ) ->
            HighlightNone


id : Card -> String
id (Card idPrefix item) =
    idPrefix ++ TaskItem.id item


subtasks : Card -> List ( String, TaskItem )
subtasks (Card idPrefix item) =
    TaskItem.subtasks item
        |> List.map (\sub -> ( idPrefix ++ TaskItem.id sub, item ))


taskItem : Card -> TaskItem
taskItem (Card _ item) =
    item
