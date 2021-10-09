module InteropDefinitions exposing
    ( Flags
    , FromElm(..)
    , ToElm(..)
    , addFilePreviewHoversEncoder
    , deleteTodoEncoder
    , displayTodoMarkdownEncoder
    , interop
    , openTodoSourceFileEncoder
    , updateTodosEncoder
    )

import Json.Decode as JD
import Json.Encode as JE
import MarkdownFile exposing (MarkdownFile)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (optional, required)


type FromElm
    = AddFilePreviewHovers { filePath : String, ids : List String }
    | DeleteTodo { filePath : String, lineNumber : Int, originalText : String }
    | DisplayTodoMarkdown { filePath : String, todoMarkdown : List { id : String, markdown : String } }
    | OpenTodoSourceFile { filePath : String, blockLink : Maybe String, lineNumber : Int, originalText : String }
    | UpdateTodos { filePath : String, todos : List { lineNumber : Int, originalText : String, newText : String } }


type ToElm
    = FileAdded MarkdownFile
    | FileDeleted String
    | FileUpdated MarkdownFile


type alias Flags =
    { folder : String
    , format : String
    , now : Int
    , zone : Int
    }


interop : { toElm : TsDecode.Decoder ToElm, fromElm : TsEncode.Encoder FromElm, flags : TsDecode.Decoder Flags }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }



-- ENCODERS


addFilePreviewHoversEncoder : TsEncode.Encoder { filePath : String, ids : List String }
addFilePreviewHoversEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "ids" .ids (TsEncode.list TsEncode.string)
        ]


deleteTodoEncoder : TsEncode.Encoder { filePath : String, lineNumber : Int, originalText : String }
deleteTodoEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        ]


displayTodoMarkdownEncoder : TsEncode.Encoder { filePath : String, todoMarkdown : List { id : String, markdown : String } }
displayTodoMarkdownEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "todoMarkdown" .todoMarkdown (TsEncode.list markdownListEncoder)
        ]


openTodoSourceFileEncoder : TsEncode.Encoder { filePath : String, blockLink : Maybe String, lineNumber : Int, originalText : String }
openTodoSourceFileEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "blockLink" .blockLink <| TsEncode.maybe TsEncode.string
        , required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        ]


updateTodosEncoder : TsEncode.Encoder { filePath : String, todos : List { lineNumber : Int, originalText : String, newText : String } }
updateTodosEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "todos" .todos (TsEncode.list todoUpdatesEncoder)
        ]



-- INTEROP


flags : TsDecode.Decoder Flags
flags =
    TsDecode.succeed Flags
        |> TsDecode.andMap (TsDecode.field "folder" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "format" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "now" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "zone" TsDecode.int)


toElm : TsDecode.Decoder ToElm
toElm =
    TsDecode.oneOf
        [ toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , toElmVariant "fileUpdated" FileUpdated MarkdownFile.decoder
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAddFilePreviewHovers vDeleteTodo vDisplayTodoMarkdown vOpenTodoSourceFile vUpdateTodos value ->
            case value of
                AddFilePreviewHovers info ->
                    vAddFilePreviewHovers info

                DeleteTodo info ->
                    vDeleteTodo info

                DisplayTodoMarkdown info ->
                    vDisplayTodoMarkdown info

                OpenTodoSourceFile info ->
                    vOpenTodoSourceFile info

                UpdateTodos info ->
                    vUpdateTodos info
        )
        |> TsEncode.variantTagged "addFilePreviewHovers" addFilePreviewHoversEncoder
        |> TsEncode.variantTagged "deleteTodo" deleteTodoEncoder
        |> TsEncode.variantTagged "displayTodoMarkdown" displayTodoMarkdownEncoder
        |> TsEncode.variantTagged "openTodoSourceFile" openTodoSourceFileEncoder
        |> TsEncode.variantTagged "updateTodos" updateTodosEncoder
        |> TsEncode.buildUnion



-- HELPERS


markdownListEncoder : TsEncode.Encoder { id : String, markdown : String }
markdownListEncoder =
    TsEncode.object
        [ required "id" .id TsEncode.string
        , required "markdown" .markdown TsEncode.string
        ]


todoUpdatesEncoder : TsEncode.Encoder { lineNumber : Int, originalText : String, newText : String }
todoUpdatesEncoder =
    TsEncode.object
        [ required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        , required "newText" .newText TsEncode.string
        ]


toElmVariant : String -> (value -> a) -> TsDecode.Decoder value -> TsDecode.Decoder a
toElmVariant tagName constructor decoder_ =
    TsDecode.field "tag" (TsDecode.literal constructor (JE.string tagName))
        |> TsDecode.andMap (TsDecode.field "data" decoder_)
