module InteropDefinitions exposing
    ( Flags
    , FromElm(..)
    , ToElm(..)
    , addFilePreviewHoversEncoder
    , deleteTaskEncoder
    , displayTaskMarkdownEncoder
    , interop
    , openTaskSourceFileEncoder
    , showCardContextMenuEncoder
    , trackDraggableEncoder
    , updateTasksEncoder
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import DecodeHelpers
import DragAndDrop.Coords as Coords exposing (Coords)
import DragAndDrop.DragData as DragData exposing (DragData)
import Filter exposing (Filter)
import MarkdownFile exposing (MarkdownFile)
import Settings exposing (Settings)
import TaskList exposing (TaskList)
import TextDirection exposing (TextDirection)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (required)


type FromElm
    = AddFilePreviewHovers (List { filePath : String, id : String })
    | CloseView
    | DeleteTask { filePath : String, lineNumber : Int, originalText : String }
    | DisplayTaskMarkdown (List { filePath : String, taskMarkdown : List { id : String, markdown : String } })
    | ElmInitialized String
    | OpenTaskSourceFile { filePath : String, lineNumber : Int, originalText : String }
    | RequestFilterCandidates
    | ShowCardContextMenu { clientPos : ( Float, Float ), cardId : String }
    | TrackDraggable { dragType : String, clientPos : Coords, draggableId : String }
    | UpdateTasks { filePath : String, tasks : List { lineNumber : Int, originalText : String, newText : String } }


type ToElm
    = ActiveStateUpdated Bool
    | AddTaskItems TaskList
    | ConfigChanged TextDirection
    | EditCardDueDate String
    | ElementDragged DragData
    | FilterCandidates (List Filter)
    | LoadTaskItems TaskList
    | SettingsUpdated Settings
    | ShowBoard Int


type alias Flags =
    { dataviewTaskCompletion : DataviewTaskCompletion
    , firstDayOfWeek : Int
    , now : Int
    , rightToLeft : Bool
    , settings : Settings
    , uniqueId : String
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


openTaskSourceFileEncoder : TsEncode.Encoder { a | filePath : String, lineNumber : Int, originalText : String }
openTaskSourceFileEncoder =
    TsEncode.object
        [ required "filePath" .filePath TsEncode.string
        , required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        ]


showCardContextMenuEncoder : TsEncode.Encoder { a | clientPos : ( Float, Float ), cardId : String }
showCardContextMenuEncoder =
    TsEncode.object
        [ required "clientPos" .clientPos (TsEncode.tuple TsEncode.float TsEncode.float)
        , required "cardId" .cardId TsEncode.string
        ]


trackDraggableEncoder : TsEncode.Encoder { dragType : String, clientPos : Coords, draggableId : String }
trackDraggableEncoder =
    TsEncode.object
        [ required "dragType" .dragType TsEncode.string
        , required "clientPos" .clientPos Coords.encoder
        , required "draggableId" .draggableId TsEncode.string
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
        |> TsDecode.andMap (TsDecode.field "dataviewTaskCompletion" DataviewTaskCompletion.decoder)
        |> TsDecode.andMap (TsDecode.field "firstDayOfWeek" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "now" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "rightToLeft" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "settings" Settings.decoder)
        |> TsDecode.andMap (TsDecode.field "uniqueId" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "zone" TsDecode.int)


toElm : TsDecode.Decoder ToElm
toElm =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "activeStateUpdated" ActiveStateUpdated TsDecode.bool
        , DecodeHelpers.toElmVariant "addTaskItems" AddTaskItems TaskList.decoder
        , DecodeHelpers.toElmVariant "configChanged" ConfigChanged configChangedDecoder
        , DecodeHelpers.toElmVariant "editCardDueDate" EditCardDueDate TsDecode.string
        , DecodeHelpers.toElmVariant "elementDragged" ElementDragged DragData.decoder
        , DecodeHelpers.toElmVariant "filterCandidates" FilterCandidates (TsDecode.list Filter.decoder)
        , DecodeHelpers.toElmVariant "loadTaskItems" LoadTaskItems TaskList.decoder
        , DecodeHelpers.toElmVariant "settingsUpdated" SettingsUpdated Settings.decoder
        , DecodeHelpers.toElmVariant "showBoard" ShowBoard TsDecode.int
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAddFilePreviewHovers vCloseView vDeleteTask vDisplayTaskMarkdown vElmInitialized vOpenTaskSourceFile vRequestPaths vShowCardContextMenu vTrackDraggable _ vUpdateTasks value ->
            case value of
                AddFilePreviewHovers info ->
                    vAddFilePreviewHovers info

                CloseView ->
                    vCloseView

                DeleteTask info ->
                    vDeleteTask info

                DisplayTaskMarkdown info ->
                    vDisplayTaskMarkdown info

                ElmInitialized uniqueId ->
                    vElmInitialized uniqueId

                OpenTaskSourceFile info ->
                    vOpenTaskSourceFile info

                RequestFilterCandidates ->
                    vRequestPaths

                ShowCardContextMenu info ->
                    vShowCardContextMenu info

                TrackDraggable info ->
                    vTrackDraggable info

                UpdateTasks info ->
                    vUpdateTasks info
        )
        |> TsEncode.variantTagged "addFilePreviewHovers" addFilePreviewHoversEncoder
        |> TsEncode.variant0 "closeView"
        |> TsEncode.variantTagged "deleteTask" deleteTaskEncoder
        |> TsEncode.variantTagged "displayTaskMarkdown" displayTaskMarkdownEncoder
        |> TsEncode.variantTagged "elmInitialized" TsEncode.string
        |> TsEncode.variantTagged "openTaskSourceFile" openTaskSourceFileEncoder
        |> TsEncode.variant0 "requestFilterCandidates"
        |> TsEncode.variantTagged "showCardContextMenu" showCardContextMenuEncoder
        |> TsEncode.variantTagged "trackDraggable" trackDraggableEncoder
        |> TsEncode.variantTagged "updateSettings" Settings.encoder
        |> TsEncode.variantTagged "updateTasks" updateTasksEncoder
        |> TsEncode.buildUnion



-- HELPERS


configChangedDecoder : TsDecode.Decoder TextDirection
configChangedDecoder =
    TsDecode.succeed TextDirection.fromRtlFlag
        |> TsDecode.andMap (TsDecode.field "rightToLeft" TsDecode.bool)


markdownListEncoder : TsEncode.Encoder { id : String, markdown : String }
markdownListEncoder =
    TsEncode.object
        [ required "id" .id TsEncode.string
        , required "markdown" .markdown TsEncode.string
        ]


renamedFileDecoder : TsDecode.Decoder ( String, String )
renamedFileDecoder =
    TsDecode.succeed Tuple.pair
        |> TsDecode.andMap (TsDecode.field "oldPath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "newPath" TsDecode.string)


taskUpdatesEncoder : TsEncode.Encoder { lineNumber : Int, originalText : String, newText : String }
taskUpdatesEncoder =
    TsEncode.object
        [ required "lineNumber" .lineNumber TsEncode.int
        , required "originalText" .originalText TsEncode.string
        , required "newText" .newText TsEncode.string
        ]
