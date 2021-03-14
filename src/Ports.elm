port module Ports exposing
    ( DataForElm(..)
    , receiveDataFromTypescript
    )

-- MODEL


type DataForElm
    = MarkdownToParse MarkdownFile


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , fileContents : String
    }


type alias GenericTypescriptData =
    { tag : String
    , data : MarkdownFile
    }



-- INTERFACE


receiveDataFromTypescript : (DataForElm -> msg) -> (String -> msg) -> Sub msg
receiveDataFromTypescript tagger onError =
    dataForElm <|
        \typescriptData ->
            case typescriptData.tag of
                "MarkdownToParse" ->
                    tagger <| MarkdownToParse typescriptData.data

                _ ->
                    onError "unexpected data from Typescript"



-- PORTS


port dataForElm : (GenericTypescriptData -> msg) -> Sub msg
