port module Ports exposing
    ( MarkdownFile
    , fileAdded
    , fileDeleted
    , fileUpdated
    )

-- TYPES


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , fileContents : String
    }



-- PORTS


port fileUpdated : (MarkdownFile -> msg) -> Sub msg


port fileAdded : (MarkdownFile -> msg) -> Sub msg


port fileDeleted : (String -> msg) -> Sub msg
