module MarkdownFile exposing (MarkdownFile, decoder)

import TagList exposing (TagList)
import TsJson.Decode as TsDecode
import Yaml.Decode as YD


type alias MarkdownFile =
    { filePath : String
    , fileDate : Maybe String
    , frontMatterTags : TagList
    , bodyOffset : Int
    , body : String
    }


decoder : TsDecode.Decoder MarkdownFile
decoder =
    TsDecode.succeed markdownFileBuilder
        |> TsDecode.andMap (TsDecode.field "filePath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "fileDate" <| TsDecode.nullable TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "fileContents" tagsAndContentsDecoder)



-- INTERNAL


markdownFileBuilder : String -> Maybe String -> ( TagList, Int, String ) -> MarkdownFile
markdownFileBuilder filePath fileDate ( tags, bodyOffset, body ) =
    MarkdownFile filePath fileDate tags bodyOffset body


tagsAndContentsDecoder : TsDecode.Decoder ( TagList, Int, String )
tagsAndContentsDecoder =
    TsDecode.andThen (TsDecode.andThenInit contentDecoder) TsDecode.string


contentDecoder : String -> TsDecode.Decoder ( TagList, Int, String )
contentDecoder contents =
    case frontMatterAndBodyFrom contents of
        ( Just frontMatter, bodyOffset, Just body ) ->
            case YD.fromString frontMatterDecoder frontMatter of
                Ok tags ->
                    TsDecode.succeed ( TagList.fromList tags, bodyOffset, body )

                Err _ ->
                    TsDecode.succeed ( TagList.empty, bodyOffset, body )

        ( Nothing, _, Just _ ) ->
            TsDecode.succeed ( TagList.empty, 0, contents )

        ( Just _, _, Nothing ) ->
            TsDecode.succeed ( TagList.empty, 0, "" )

        ( Nothing, _, Nothing ) ->
            TsDecode.succeed ( TagList.empty, 0, "" )


frontMatterDecoder : YD.Decoder (List String)
frontMatterDecoder =
    YD.field "tags" (YD.list YD.string)


frontMatterAndBodyFrom : String -> ( Maybe String, Int, Maybe String )
frontMatterAndBodyFrom contents =
    let
        splitContents : List String
        splitContents =
            String.split "---" contents
    in
    if noFrontMatter splitContents then
        ( Nothing, 0, Just contents )

    else if hasFrontMatter splitContents then
        let
            bodyOffset : Int
            bodyOffset =
                splitContents
                    |> List.take 2
                    |> String.join ""
                    |> String.lines
                    |> List.length
        in
        ( (List.head << List.drop 1) splitContents
        , bodyOffset
        , Just <| (String.dropLeft 1 << String.join "" << List.drop 2) splitContents
        )

    else
        ( Nothing, 0, Nothing )


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
