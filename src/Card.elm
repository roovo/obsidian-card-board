module Card exposing
    ( Card
    , Highlight(..)
    , editButtonId
    , fromTaskItem
    , highlight
    , id
    , markdownWithIds
    , notesId
    , subtasks
    , taskItem
    , taskItemId
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
id ((Card idPrefix _) as card) =
    idPrefix ++ taskItemId card


editButtonId : Card -> String
editButtonId card =
    id card ++ ":editButton"


markdownWithIds : Card -> List { id : String, markdown : String }
markdownWithIds card =
    let
        item =
            taskItem card

        subtaskMarkdownWithId ( subtaskId, subtask ) =
            { id = subtaskId
            , markdown = TaskItem.title subtask
            }

        subtasksWithIds =
            card
                |> subtasks
                |> List.map subtaskMarkdownWithId

        notesWithId =
            if TaskItem.hasNotes item then
                [ { id = notesId card
                  , markdown = TaskItem.notes item
                  }
                ]

            else
                []

        markdownWithId c =
            [ { id = id c
              , markdown = TaskItem.title item
              }
            ]
    in
    card
        |> markdownWithId
        |> List.append subtasksWithIds
        |> List.append notesWithId


notesId : Card -> String
notesId card =
    id card ++ ":notes"


subtasks : Card -> List ( String, TaskItem )
subtasks (Card idPrefix item) =
    TaskItem.subtasks item
        |> List.map (\sub -> ( idPrefix ++ TaskItem.id sub, sub ))


taskItem : Card -> TaskItem
taskItem (Card _ item) =
    item


taskItemId : Card -> String
taskItemId (Card _ item) =
    TaskItem.id item
