module Card exposing
    ( Card
    , Highlight(..)
    , editButtonId
    , filePath
    , fromTaskItem
    , highlight
    , id
    , markdownWithIds
    , notesId
    , subtasks
    , taskItem
    , taskItemId
    )

import Date
import TaskItem exposing (TaskItem)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Card
    = Card String TaskItem


type Highlight
    = HighlightNone
    | HighlightCritical
    | HighlightGood
    | HighlightImportant



-- CONSTRUCTION


fromTaskItem : String -> TaskItem -> Card
fromTaskItem =
    Card



-- INFO


highlight : TimeWithZone -> Card -> Highlight
highlight timeWithZone (Card _ item) =
    let
        datestamp =
            TimeWithZone.toDate timeWithZone
    in
    case ( TaskItem.isCompleted item, TaskItem.due item ) of
        ( False, Just dueDate ) ->
            if datestamp == dueDate then
                HighlightImportant

            else if Date.toRataDie datestamp > Date.toRataDie dueDate then
                HighlightCritical

            else if Date.toRataDie datestamp < Date.toRataDie dueDate then
                HighlightGood

            else
                HighlightNone

        _ ->
            HighlightNone


id : Card -> String
id ((Card idPrefix _) as card) =
    idPrefix ++ taskItemId card


editButtonId : Card -> String
editButtonId card =
    id card ++ ":editButton"


filePath : Card -> String
filePath (Card _ item) =
    TaskItem.filePath item


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
