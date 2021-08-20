port module Ports exposing
    ( MarkdownFile
    , deleteTodo
    , displayTaskTitles
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


displayTaskTitles : String -> TaskList -> Cmd msg
displayTaskTitles filePath taskList =
    let
        titleWithId t =
            { id = TaskItem.id t
            , titleMarkdown = TaskItem.title t
            }

        titlesWithIds =
            taskList
                |> TaskList.tasks
                |> List.map titleWithId
    in
    displayTitles { filePath = filePath, titles = titlesWithIds }


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


port displayTitles : { filePath : String, titles : List { id : String, titleMarkdown : String } } -> Cmd msg


port editTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg


port updateTodos : { filePath : String, todos : List { lineNumber : Int, originalText : String, newText : String } } -> Cmd msg



-- PORTS, INBOUND


port fileAdded : (MarkdownFile -> msg) -> Sub msg


port fileDeleted : (String -> msg) -> Sub msg


port fileUpdated : (MarkdownFile -> msg) -> Sub msg
