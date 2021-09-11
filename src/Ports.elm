port module Ports exposing
    ( MarkdownFile
    , addHoverToCardEditButtons
    , deleteTodo
    , displayTaskMarkdown
    , fileAdded
    , fileDeleted
    , fileUpdated
    , openTodoSourceFile
    , rewriteTodos
    )

import CardBoard exposing (CardBoard)
import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , fileContents : String
    }



-- HELPERS


addHoverToCardEditButtons : String -> CardBoard -> Time.Posix -> Time.Zone -> TaskList -> Cmd msg
addHoverToCardEditButtons filePath cardBoard now zone taskList =
    let
        inColumnLinkIds : List String
        inColumnLinkIds =
            cardBoard
                |> CardBoard.columns now zone taskList
                |> List.concatMap Tuple.second
                |> List.concatMap (\i -> i :: TaskItem.subtasks i)
                |> List.map TaskItem.inColumnId
                |> List.map (\id -> id ++ ":editButton")
    in
    addFilePreviewHovers { filePath = filePath, ids = inColumnLinkIds }


displayTaskMarkdown : String -> CardBoard -> Time.Posix -> Time.Zone -> TaskList -> Cmd msg
displayTaskMarkdown filePath cardBoard now zone taskList =
    let
        taskItems : List TaskItem
        taskItems =
            cardBoard
                |> CardBoard.columns now zone taskList
                |> List.concatMap Tuple.second
                |> List.concatMap (\i -> i :: TaskItem.subtasks i)

        markdownWithIds =
            taskItems
                |> List.map markdownWithId
                |> List.append notesWithIds

        markdownWithId t =
            { id = TaskItem.inColumnId t
            , markdown = TaskItem.title t
            }

        notesMarkdownWithId t =
            { id = TaskItem.inColumnId t ++ ":notes"
            , markdown = TaskItem.notes t
            }

        notesWithIds =
            taskItems
                |> List.filter (\t -> TaskItem.hasNotes t)
                |> List.map notesMarkdownWithId
    in
    displayTodoMarkdown { filePath = filePath, todoMarkdown = markdownWithIds }


rewriteTodos : Time.Posix -> String -> List TaskItem -> Cmd msg
rewriteTodos now filePath taskItems =
    let
        buildFoo taskItem =
            { lineNumber = TaskItem.lineNumber taskItem
            , originalText = TaskItem.originalText taskItem
            , newText = taskItem |> TaskItem.toggleCompletion now |> TaskItem.toString
            }
    in
    updateTodos { filePath = filePath, todos = List.map buildFoo taskItems }



-- PORTS, OUTBOUND


port addFilePreviewHovers : { filePath : String, ids : List String } -> Cmd msg


port deleteTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg


port displayTodoMarkdown : { filePath : String, todoMarkdown : List { id : String, markdown : String } } -> Cmd msg


port openTodoSourceFile : { filePath : String, blockLink : Maybe String, lineNumber : Int, originalText : String } -> Cmd msg


port updateTodos : { filePath : String, todos : List { lineNumber : Int, originalText : String, newText : String } } -> Cmd msg



-- PORTS, INBOUND


port fileAdded : (MarkdownFile -> msg) -> Sub msg


port fileDeleted : (String -> msg) -> Sub msg


port fileUpdated : (MarkdownFile -> msg) -> Sub msg
