port module Ports exposing
    ( MarkdownFile
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


port toggleTodo : { filePath : String, lineNumber : Int, title : String, setToChecked : Bool } -> Cmd msg


port fileUpdated : (MarkdownFile -> msg) -> Sub msg


port fileAdded : (MarkdownFile -> msg) -> Sub msg


port fileDeleted : (String -> msg) -> Sub msg
