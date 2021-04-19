port module Ports exposing
    ( MarkdownFile
    , deleteTodo
    , editTodo
    , fileAdded
    , fileDeleted
    , fileUpdated
    , toggleTodo
    )

-- TYPES


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , fileContents : String
    }



-- PORTS


port deleteTodo : { filePath : String, lineNumber : Int, title : String } -> Cmd msg


port editTodo : { filePath : String, lineNumber : Int, title : String } -> Cmd msg


port toggleTodo : { filePath : String, lineNumber : Int, title : String, setToChecked : Bool } -> Cmd msg


port fileUpdated : (MarkdownFile -> msg) -> Sub msg


port fileAdded : (MarkdownFile -> msg) -> Sub msg


port fileDeleted : (String -> msg) -> Sub msg
