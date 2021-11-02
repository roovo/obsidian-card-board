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

import BoardConfig exposing (BoardConfig)
import CardBoardSettings exposing (Settings)
import DateBoard
import DecodeHelpers
import Json.Encode as JE
import MarkdownFile exposing (MarkdownFile)
import Semver
import TagBoard
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (required)


type FromElm
    = AddFilePreviewHovers (List { filePath : String, id : String })
    | CloseView
    | DeleteTodo { filePath : String, lineNumber : Int, originalText : String }
    | DisplayTodoMarkdown (List { filePath : String, todoMarkdown : List { id : String, markdown : String } })
    | OpenTodoSourceFile { filePath : String, lineNumber : Int, originalText : String }
    | UpdateSettings Settings
    | UpdateTodos { filePath : String, todos : List { lineNumber : Int, originalText : String, newText : String } }


type ToElm
    = FileAdded MarkdownFile
    | FileDeleted String
    | FileRenamed ( String, String )
    | FileUpdated MarkdownFile
    | InitCompleted
    | SettingsUpdated Settings


type alias Flags =
    { settings : CardBoardSettings.Settings
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


addFilePreviewHoversEncoder : TsEncode.Encoder (List { filePath : String, id : String })
addFilePreviewHoversEncoder =
    TsEncode.list
        (TsEncode.object
            [ required "filePath" .filePath TsEncode.string
            , required "id" .id TsEncode.string
            ]
        )


closeViewEncoder : JE.Value
closeViewEncoder =
    JE.object
        [ ( "tag", JE.string "closeView" ) ]


deleteTodoEncoder : TsEncode.Encoder { filePath : String, lineNumber : Int, originalText : String }
deleteTodoEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        ]


displayTodoMarkdownEncoder : TsEncode.Encoder (List { filePath : String, todoMarkdown : List { id : String, markdown : String } })
displayTodoMarkdownEncoder =
    TsEncode.list
        (TsEncode.object
            [ required "filePath" .filePath TsEncode.string
            , required "todoMarkdown" .todoMarkdown (TsEncode.list markdownListEncoder)
            ]
        )


openTodoSourceFileEncoder : TsEncode.Encoder { filePath : String, lineNumber : Int, originalText : String }
openTodoSourceFileEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
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
        |> TsDecode.andMap (TsDecode.field "settings" CardBoardSettings.decoder)
        |> TsDecode.andMap (TsDecode.field "now" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "zone" TsDecode.int)


toElm : TsDecode.Decoder ToElm
toElm =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , DecodeHelpers.toElmVariant "fileRenamed" FileRenamed renamedFileDecoder
        , DecodeHelpers.toElmVariant "fileUpdated" FileUpdated MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "settingsUpdated" SettingsUpdated CardBoardSettings.decoder
        , DecodeHelpers.toElmVariant "initCompleted" (always InitCompleted) (TsDecode.succeed ())
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAddFilePreviewHovers vCloseView vDeleteTodo vDisplayTodoMarkdown vOpenTodoSourceFile vUpdateConfig vUpdateTodos value ->
            case value of
                AddFilePreviewHovers info ->
                    vAddFilePreviewHovers info

                CloseView ->
                    vCloseView

                DeleteTodo info ->
                    vDeleteTodo info

                DisplayTodoMarkdown info ->
                    vDisplayTodoMarkdown info

                OpenTodoSourceFile info ->
                    vOpenTodoSourceFile info

                UpdateSettings info ->
                    vUpdateConfig info

                UpdateTodos info ->
                    vUpdateTodos info
        )
        |> TsEncode.variantTagged "addFilePreviewHovers" addFilePreviewHoversEncoder
        |> TsEncode.variant0 "closeView"
        |> TsEncode.variantTagged "deleteTodo" deleteTodoEncoder
        |> TsEncode.variantTagged "displayTodoMarkdown" displayTodoMarkdownEncoder
        |> TsEncode.variantTagged "openTodoSourceFile" openTodoSourceFileEncoder
        |> TsEncode.variantTagged "updateSettings" CardBoardSettings.encoder
        |> TsEncode.variantTagged "updateTodos" updateTodosEncoder
        |> TsEncode.buildUnion


renamedFileDecoder : TsDecode.Decoder ( String, String )
renamedFileDecoder =
    TsDecode.succeed Tuple.pair
        |> TsDecode.andMap (TsDecode.field "oldPath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "newPath" TsDecode.string)



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
