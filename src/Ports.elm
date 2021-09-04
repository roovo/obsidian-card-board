port module Ports exposing
    ( MarkdownFile
    , deleteTodo
    , displayTaskMarkdown
    , editTodo
    , fileAdded
    , fileDeleted
    , fileUpdated
    , rewriteTodos
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , fileContents : String
    }



-- HELPERS


displayTaskMarkdown : String -> TaskList -> Cmd msg
displayTaskMarkdown filePath taskList =
    let
        markdownWithId t =
            { id = TaskItem.id t
            , markdown = TaskItem.title t
            }

        notesMarkdownWithId t =
            { id = TaskItem.id t ++ ":notes"
            , markdown = TaskItem.notes t
            }

        notesWithIds =
            taskList
                |> TaskList.tasks
                |> List.filter (\t -> TaskItem.hasNotes t)
                |> List.map notesMarkdownWithId

        markdownWithIds =
            taskList
                |> TaskList.tasks
                |> List.map markdownWithId
                |> List.append notesWithIds
    in
    displayTodoMarkdown { filePath = filePath, todoMarkdown = markdownWithIds }


rewriteTodos : Maybe Date -> String -> List TaskItem -> Cmd msg
rewriteTodos today filePath taskItems =
    let
        buildFoo taskItem =
            { lineNumber = TaskItem.lineNumber taskItem
            , originalText = TaskItem.originalText taskItem
            , newText = taskItem |> TaskItem.toggleCompletion today |> TaskItem.toString
            }
    in
    updateTodos { filePath = filePath, todos = List.map buildFoo taskItems }



-- PORTS, OUTBOUND


port deleteTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg


port displayTodoMarkdown : { filePath : String, todoMarkdown : List { id : String, markdown : String } } -> Cmd msg


port editTodo : { filePath : String } -> Cmd msg


port updateTodos : { filePath : String, todos : List { lineNumber : Int, originalText : String, newText : String } } -> Cmd msg



-- PORTS, INBOUND


port fileAdded : (MarkdownFile -> msg) -> Sub msg


port fileDeleted : (String -> msg) -> Sub msg


port fileUpdated : (MarkdownFile -> msg) -> Sub msg
