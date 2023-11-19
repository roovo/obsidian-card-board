module Card exposing
    ( Card
    , Highlight(..)
    , allTags
    , descendantTasks
    , displayTags
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
import TagList exposing (TagList)
import TaskItem exposing (TaskItem)



-- TYPES


type Card
    = Card String TagList TaskItem


type Highlight
    = HighlightNone
    | HighlightCritical
    | HighlightGood
    | HighlightImportant



-- CONSTRUCTION


fromTaskItem : String -> List String -> TaskItem -> Card
fromTaskItem idPrefix tagsToHide taskItem_ =
    Card idPrefix (TaskItem.tags taskItem_) (TaskItem.removeTags tagsToHide taskItem_)



-- INFO


highlight : Date -> Card -> Highlight
highlight datestamp (Card _ _ item) =
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
id ((Card idPrefix _ _) as card) =
    idPrefix ++ ":" ++ taskItemId card


editButtonId : Card -> String
editButtonId card =
    id card ++ ":editButton"


filePath : Card -> String
filePath (Card _ _ item) =
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
                TaskItem.titleWithTags subtask
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
                topLevelTagsMarkdown : String
                topLevelTagsMarkdown =
                    TaskItem.topLevelTags item
                        |> TagList.toList
                        |> List.map (String.append "#")
                        |> String.join " "
            in
            if TaskItem.hasTopLevelTags item then
                [ { id = tagsId card
                  , markdown = topLevelTagsMarkdown
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


allTags : Card -> TagList
allTags (Card _ tagList _) =
    tagList


displayTags : Card -> TagList
displayTags =
    TaskItem.tags << taskItem


tagsId : Card -> String
tagsId card =
    id card ++ ":tags"


descendantTasks : Card -> List ( String, TaskItem )
descendantTasks (Card idPrefix _ item) =
    TaskItem.descendantTasks item
        |> List.map (\sub -> ( idPrefix ++ ":" ++ TaskItem.id sub, sub ))


taskItem : Card -> TaskItem
taskItem (Card _ _ item) =
    item


taskItemId : Card -> String
taskItemId (Card _ _ item) =
    TaskItem.id item
