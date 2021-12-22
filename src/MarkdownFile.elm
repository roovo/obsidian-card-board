module MarkdownFile exposing (MarkdownFile, decoder)

import TsJson.Decode as TsDecode
import Yaml.Decode as YD


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , frontMatterTags : List String
    , fileContents : String
    }


decoder : TsDecode.Decoder MarkdownFile
decoder =
    TsDecode.succeed markdownFileBuilder
        |> TsDecode.andMap (TsDecode.field "filePath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "fileDate" <| TsDecode.nullable TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "fileContents" tagsAndContentsDecoder)



-- INTERNAL


markdownFileBuilder : String -> Maybe String -> ( List String, String ) -> MarkdownFile
markdownFileBuilder filePath fileDate ( tags, content ) =
    MarkdownFile filePath fileDate tags content


tagsAndContentsDecoder : TsDecode.Decoder ( List String, String )
tagsAndContentsDecoder =
    TsDecode.andThen (TsDecode.andThenInit contentDecoder) TsDecode.string


contentDecoder : String -> TsDecode.Decoder ( List String, String )
contentDecoder contents =
    case extractFrontMatter contents of
        ( Just frontMatter, Just body ) ->
            case YD.fromString frontMatterDecoder frontMatter of
                Ok tags ->
                    TsDecode.succeed ( tags, body )

                Err _ ->
                    TsDecode.succeed ( [], body )

        ( Nothing, Just _ ) ->
            TsDecode.succeed ( [], contents )

        ( Just _, Nothing ) ->
            TsDecode.succeed ( [], "" )

        ( Nothing, Nothing ) ->
            TsDecode.succeed ( [], "" )


frontMatterDecoder : YD.Decoder (List String)
frontMatterDecoder =
    YD.field "tags" (YD.list YD.string)


extractFrontMatter : String -> ( Maybe String, Maybe String )
extractFrontMatter contents =
    let
        splitContents =
            String.split "---" contents
    in
    if noFrontMatter splitContents then
        ( Nothing, Just contents )

    else if hasFrontMatter splitContents then
        ( (List.head << List.drop 1) splitContents
        , Just <| (String.trim << String.join "" << List.drop 2) splitContents
        )

    else
        ( Nothing, Nothing )


hasFrontMatter : List String -> Bool
hasFrontMatter splitContents =
    startsWithWhiteSpace splitContents && List.length splitContents > 2


startsWithWhiteSpace : List String -> Bool
startsWithWhiteSpace splitContents =
    List.head splitContents
        |> Maybe.map String.trim
        |> Maybe.map String.isEmpty
        |> Maybe.withDefault False


noFrontMatter : List String -> Bool
noFrontMatter =
    not << startsWithWhiteSpace
