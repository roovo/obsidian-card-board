module MarkdownFile exposing (MarkdownFile, decoder)

import TsJson.Decode as TsDecode


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , fileContents : String
    }


decoder : TsDecode.Decoder MarkdownFile
decoder =
    TsDecode.succeed MarkdownFile
        |> TsDecode.andMap (TsDecode.field "filePath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "fileDate" <| TsDecode.nullable TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "fileContents" TsDecode.string)
