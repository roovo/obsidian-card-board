module InteropDefinitions exposing
    ( Flags
    , FromElm(..)
    , ToElm(..)
    , addFilePreviewHoversEncoder
    , deleteTodoEncoder
    , displayTodoMarkdownEncoder
    , interop
    , openTodoSourceFileEncoder
    , updateSettingsEncoder
    , updateTodosEncoder
    )

import CardBoard
import DateBoard
import Json.Decode as JD
import Json.Encode as JE
import MarkdownFile exposing (MarkdownFile)
import Semver
import TagBoard
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (optional, required)


type FromElm
    = AddFilePreviewHovers (List { filePath : String, id : String })
    | DeleteTodo { filePath : String, lineNumber : Int, originalText : String }
    | DisplayTodoMarkdown (List { filePath : String, todoMarkdown : List { id : String, markdown : String } })
    | OpenTodoSourceFile { filePath : String, blockLink : Maybe String, lineNumber : Int, originalText : String }
    | UpdateSettings CardBoard.Settings
    | UpdateTodos { filePath : String, todos : List { lineNumber : Int, originalText : String, newText : String } }


type ToElm
    = FileAdded MarkdownFile
    | FileDeleted String
    | FileRenamed ( String, String )
    | FileUpdated MarkdownFile
    | InitCompleted ()
    | SettingsUpdated CardBoard.Settings


type alias Flags =
    { boardConfigs : List CardBoard.Config
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


openTodoSourceFileEncoder : TsEncode.Encoder { filePath : String, blockLink : Maybe String, lineNumber : Int, originalText : String }
openTodoSourceFileEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "blockLink" .blockLink <| TsEncode.maybe TsEncode.string
        , required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        ]


updateSettingsEncoder : TsEncode.Encoder CardBoard.Settings
updateSettingsEncoder =
    TsEncode.object
        [ required "version" CardBoard.version TsEncode.string
        , required "data" CardBoard.boardConfigs boardConfigsEncoder
        ]


boardConfigsEncoder : TsEncode.Encoder { boardConfigs : List CardBoard.Config }
boardConfigsEncoder =
    TsEncode.object
        [ required "boardConfigs" .boardConfigs (TsEncode.list cardBoardConfigEncoder)
        ]


cardBoardConfigEncoder : TsEncode.Encoder CardBoard.Config
cardBoardConfigEncoder =
    TsEncode.union
        (\vDateBoardConfig vTagBoardConfig value ->
            case value of
                CardBoard.DateBoardConfig config ->
                    vDateBoardConfig config

                CardBoard.TagBoardConfig config ->
                    vTagBoardConfig config
        )
        |> TsEncode.variantTagged "dateBoardConfig" dateBoardConfigEncoder
        |> TsEncode.variantTagged "tagBoardConfig" tagBoardConfigEncoder
        |> TsEncode.buildUnion


dateBoardConfigEncoder : TsEncode.Encoder DateBoard.Config
dateBoardConfigEncoder =
    TsEncode.object
        [ required "completedCount" .completedCount TsEncode.int
        , required "includeUndated" .includeUndated TsEncode.bool
        , required "title" .title TsEncode.string
        ]


tagBoardConfigEncoder : TsEncode.Encoder TagBoard.Config
tagBoardConfigEncoder =
    TsEncode.object
        [ required "columns" .columns <| TsEncode.list tagBoardColumnConfigEncoder
        , required "completedCount" .completedCount TsEncode.int
        , required "includeOthers" .includeOthers TsEncode.bool
        , required "includeUntagged" .includeUntagged TsEncode.bool
        , required "title" .title TsEncode.string
        ]


tagBoardColumnConfigEncoder : TsEncode.Encoder TagBoard.ColumnConfig
tagBoardColumnConfigEncoder =
    TsEncode.object
        [ required "tag" .tag TsEncode.string
        , required "displayTitle" .displayTitle TsEncode.string
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
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list boardConfigDecoder))
        |> TsDecode.andMap (TsDecode.field "now" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "zone" TsDecode.int)


toElm : TsDecode.Decoder ToElm
toElm =
    TsDecode.oneOf
        [ toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , toElmVariant "fileRenamed" FileRenamed renamedFileDecoder
        , toElmVariant "fileUpdated" FileUpdated MarkdownFile.decoder
        , toElmVariant "settingsUpdated" SettingsUpdated boardSettingsDecoder
        , toElmVariant "initCompleted" InitCompleted (TsDecode.succeed ())
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAddFilePreviewHovers vDeleteTodo vDisplayTodoMarkdown vOpenTodoSourceFile vUpdateConfig vUpdateTodos value ->
            case value of
                AddFilePreviewHovers info ->
                    vAddFilePreviewHovers info

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
        |> TsEncode.variantTagged "deleteTodo" deleteTodoEncoder
        |> TsEncode.variantTagged "displayTodoMarkdown" displayTodoMarkdownEncoder
        |> TsEncode.variantTagged "openTodoSourceFile" openTodoSourceFileEncoder
        |> TsEncode.variantTagged "updateSettings" updateSettingsEncoder
        |> TsEncode.variantTagged "updateTodos" updateTodosEncoder
        |> TsEncode.buildUnion


boardSettingsDecoder : TsDecode.Decoder CardBoard.Settings
boardSettingsDecoder =
    TsDecode.field "version" TsDecode.string
        |> TsDecode.andThen versionedSettingsDecoder


renamedFileDecoder : TsDecode.Decoder ( String, String )
renamedFileDecoder =
    TsDecode.succeed Tuple.pair
        |> TsDecode.andMap (TsDecode.field "oldPath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "newPath" TsDecode.string)


versionedSettingsDecoder : TsDecode.AndThenContinuation (String -> TsDecode.Decoder CardBoard.Settings)
versionedSettingsDecoder =
    TsDecode.andThenInit
        (\v0_1_0 unsupportedVersion version ->
            case version of
                "0.1.0" ->
                    v0_1_0

                _ ->
                    unsupportedVersion
        )
        |> TsDecode.andThenDecoder (TsDecode.field "data" v0_1_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" unsupportedVersionDecoder)


v0_1_0_Decoder : TsDecode.Decoder CardBoard.Settings
v0_1_0_Decoder =
    TsDecode.succeed CardBoard.Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list boardConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed (Semver.version 0 0 0 [] []))


unsupportedVersionDecoder : TsDecode.Decoder CardBoard.Settings
unsupportedVersionDecoder =
    TsDecode.fail "Unsupported settings file version"


boardConfigDecoder : TsDecode.Decoder CardBoard.Config
boardConfigDecoder =
    TsDecode.oneOf
        [ toElmVariant "dateBoardConfig" CardBoard.DateBoardConfig dateBoardConfigDecoder
        , toElmVariant "tagBoardConfig" CardBoard.TagBoardConfig tagBoardConfigDecoder
        ]


dateBoardConfigDecoder : TsDecode.Decoder DateBoard.Config
dateBoardConfigDecoder =
    TsDecode.succeed DateBoard.Config
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


tagBoardConfigDecoder : TsDecode.Decoder TagBoard.Config
tagBoardConfigDecoder =
    TsDecode.succeed TagBoard.Config
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list tagBoardColumnConfigDecoder))
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


tagBoardColumnConfigDecoder : TsDecode.Decoder TagBoard.ColumnConfig
tagBoardColumnConfigDecoder =
    TsDecode.succeed TagBoard.ColumnConfig
        |> TsDecode.andMap (TsDecode.field "tag" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "displayTitle" TsDecode.string)



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
