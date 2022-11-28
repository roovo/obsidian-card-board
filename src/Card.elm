module Card exposing
    ( Card
    , Highlight(..)
    , descendantTasks
    , editButtonId
    , filePath
    , fromTaskItem
    , highlight
    , id
    , markdownWithIds
    , notesId
    , tagsId
    , taskItem
    , taskItemId
    )

import Date exposing (Date)
import TagList
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
        datestamp : Date
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
    idPrefix ++ ":" ++ taskItemId card


editButtonId : Card -> String
editButtonId card =
    id card ++ ":editButton"


filePath : Card -> String
filePath (Card _ item) =
    TaskItem.filePath item


markdownWithIds : Card -> List { id : String, markdown : String }
markdownWithIds card =
    let
        item : TaskItem
        item =
            taskItem card

        markdownWithId : Card -> List { id : String, markdown : String }
        markdownWithId c =
            [ { id = id c
              , markdown =
                    TaskItem.title item
                        |> strikeIfCompleted item
              }
            ]

        notesWithId : List { id : String, markdown : String }
        notesWithId =
            if TaskItem.hasNotes item then
                [ { id = notesId card
                  , markdown = TaskItem.notes item
                  }
                ]

            else
                []

        strikeIfCompleted : TaskItem -> String -> String
        strikeIfCompleted i markdown =
            if TaskItem.isCompleted i then
                "~~" ++ markdown ++ "~~"

            else
                markdown

        subtaskMarkdownWithId : ( String, TaskItem ) -> { id : String, markdown : String }
        subtaskMarkdownWithId ( subtaskId, subtask ) =
            { id = subtaskId
            , markdown =
                TaskItem.title subtask
                    |> strikeIfCompleted subtask
            }

        subtasksWithIds : List { id : String, markdown : String }
        subtasksWithIds =
            card
                |> descendantTasks
                |> List.map subtaskMarkdownWithId

        tagsWithId : List { id : String, markdown : String }
        tagsWithId =
            let
                tagsMarkdown : String
                tagsMarkdown =
                    TaskItem.tags item
                        |> TagList.toList
                        |> List.map (String.append "#")
                        |> String.join " "
            in
            if TaskItem.hasTags item then
                [ { id = tagsId card
                  , markdown = tagsMarkdown
                  }
                ]

            else
                []
    in
    card
        |> markdownWithId
        |> List.append subtasksWithIds
        |> List.append notesWithId
        |> List.append tagsWithId


notesId : Card -> String
notesId card =
    id card ++ ":notes"


tagsId : Card -> String
tagsId card =
    id card ++ ":tags"


descendantTasks : Card -> List ( String, TaskItem )
descendantTasks (Card idPrefix item) =
    TaskItem.descendantTasks item
        |> List.map (\sub -> ( idPrefix ++ ":" ++ TaskItem.id sub, sub ))


taskItem : Card -> TaskItem
taskItem (Card _ item) =
    item


taskItemId : Card -> String
taskItemId (Card _ item) =
    TaskItem.id item
