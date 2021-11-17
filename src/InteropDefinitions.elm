module InteropDefinitions exposing
    ( Flags
    , FromElm(..)
    , ToElm(..)
    , addFilePreviewHoversEncoder
    , deleteTaskEncoder
    , displayTaskMarkdownEncoder
    , globalSearchTermEncoder
    , interop
    , openTaskSourceFileEncoder
    , updateTasksEncoder
    )

import CardBoardSettings exposing (Settings)
import DecodeHelpers
import MarkdownFile exposing (MarkdownFile)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (required)


type FromElm
    = AddFilePreviewHovers (List { filePath : String, id : String })
    | DeleteTask { filePath : String, lineNumber : Int, originalText : String }
    | DisplayTaskMarkdown (List { filePath : String, taskMarkdown : List { id : String, markdown : String } })
    | GlobalSearch { searchTerm : String }
    | OpenTaskSourceFile { filePath : String, lineNumber : Int, originalText : String }
    | UpdateSettings Settings
    | UpdateTasks { filePath : String, tasks : List { lineNumber : Int, originalText : String, newText : String } }


type ToElm
    = ActiveStateUpdated Bool
    | FileAdded MarkdownFile
    | FileDeleted String
    | FileRenamed ( String, String )
    | FileUpdated MarkdownFile
    | InitCompleted
    | SettingsUpdated Settings
    | ShowBoard Int


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


deleteTaskEncoder : TsEncode.Encoder { a | filePath : String, lineNumber : Int, originalText : String }
deleteTaskEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        ]


displayTaskMarkdownEncoder : TsEncode.Encoder (List { filePath : String, taskMarkdown : List { id : String, markdown : String } })
displayTaskMarkdownEncoder =
    TsEncode.list
        (TsEncode.object
            [ required "filePath" .filePath TsEncode.string
            , required "taskMarkdown" .taskMarkdown (TsEncode.list markdownListEncoder)
            ]
        )


globalSearchTermEncoder : TsEncode.Encoder { a | searchTerm : String }
globalSearchTermEncoder =
    TsEncode.object [ required "searchTerm" .searchTerm TsEncode.string ]


openTaskSourceFileEncoder : TsEncode.Encoder { a | filePath : String, lineNumber : Int, originalText : String }
openTaskSourceFileEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        ]


updateTasksEncoder : TsEncode.Encoder { filePath : String, tasks : List { lineNumber : Int, originalText : String, newText : String } }
updateTasksEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "tasks" .tasks (TsEncode.list taskUpdatesEncoder)
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
        [ DecodeHelpers.toElmVariant "activeStateUpdated" ActiveStateUpdated TsDecode.bool
        , DecodeHelpers.toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , DecodeHelpers.toElmVariant "fileRenamed" FileRenamed renamedFileDecoder
        , DecodeHelpers.toElmVariant "fileUpdated" FileUpdated MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "initCompleted" (always InitCompleted) (TsDecode.succeed ())
        , DecodeHelpers.toElmVariant "settingsUpdated" SettingsUpdated CardBoardSettings.decoder
        , DecodeHelpers.toElmVariant "showBoard" ShowBoard TsDecode.int
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAddFilePreviewHovers _ vDeleteTask vDisplayTaskMarkdown vGlobalSearch vOpenTaskSourceFile vUpdateConfig vUpdateTasks value ->
            case value of
                AddFilePreviewHovers info ->
                    vAddFilePreviewHovers info

                DeleteTask info ->
                    vDeleteTask info

                DisplayTaskMarkdown info ->
                    vDisplayTaskMarkdown info

                GlobalSearch searchTerm ->
                    vGlobalSearch searchTerm

                OpenTaskSourceFile info ->
                    vOpenTaskSourceFile info

                UpdateSettings info ->
                    vUpdateConfig info

                UpdateTasks info ->
                    vUpdateTasks info
        )
        |> TsEncode.variantTagged "addFilePreviewHovers" addFilePreviewHoversEncoder
        |> TsEncode.variant0 "closeView"
        |> TsEncode.variantTagged "deleteTask" deleteTaskEncoder
        |> TsEncode.variantTagged "displayTaskMarkdown" displayTaskMarkdownEncoder
        |> TsEncode.variantTagged "globalSearch" globalSearchTermEncoder
        |> TsEncode.variantTagged "openTaskSourceFile" openTaskSourceFileEncoder
        |> TsEncode.variantTagged "updateSettings" CardBoardSettings.encoder
        |> TsEncode.variantTagged "updateTasks" updateTasksEncoder
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


taskUpdatesEncoder : TsEncode.Encoder { lineNumber : Int, originalText : String, newText : String }
taskUpdatesEncoder =
    TsEncode.object
        [ required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        , required "newText" .newText TsEncode.string
        ]
