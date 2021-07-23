port module Ports exposing
    ( MarkdownFile
    , deleteTodo
    , editTodo
    , fileAdded
    , fileDeleted
    , fileUpdated
    , rewriteTodo
    )

-- TYPES


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , fileContents : String
    }



-- PORTS


port deleteTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg


port editTodo : { filePath : String, lineNumber : Int, originalText : String } -> Cmd msg


port rewriteTodo : { filePath : String, lineNumber : Int, originalText : String, newText : String } -> Cmd msg


port fileUpdated : (MarkdownFile -> msg) -> Sub msg


port fileAdded : (MarkdownFile -> msg) -> Sub msg


port fileDeleted : (String -> msg) -> Sub msg
