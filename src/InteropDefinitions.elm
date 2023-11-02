module InteropDefinitions exposing
    ( DragData
    , Flags
    , FromElm(..)
    , ToElm(..)
    , addFilePreviewHoversEncoder
    , deleteTaskEncoder
    , displayTaskMarkdownEncoder
    , interop
    , openTaskSourceFileEncoder
    , trackDraggableEncoder
    , updateTasksEncoder
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import DecodeHelpers
import Filter exposing (Filter)
import MarkdownFile exposing (MarkdownFile)
import Settings exposing (Settings)
import TextDirection exposing (TextDirection)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (required)


type FromElm
    = AddFilePreviewHovers (List { filePath : String, id : String })
    | CloseView
    | DeleteTask { filePath : String, lineNumber : Int, originalText : String }
    | DisplayTaskMarkdown (List { filePath : String, taskMarkdown : List { id : String, markdown : String } })
    | ElmInitialized
    | OpenTaskSourceFile { filePath : String, lineNumber : Int, originalText : String }
    | RequestFilterCandidates
    | TrackDraggable { beaconIdentifier : String, clientPos : ( Float, Float ) }
    | UpdateTasks { filePath : String, tasks : List { lineNumber : Int, originalText : String, newText : String } }


type ToElm
    = ActiveStateUpdated Bool
    | ConfigChanged TextDirection
    | ElementDragged DragData
    | FileAdded MarkdownFile
    | FileDeleted String
    | FileRenamed ( String, String )
    | FileUpdated MarkdownFile
    | FilterCandidates (List Filter)
    | AllMarkdownLoaded
    | SettingsUpdated Settings
    | ShowBoard Int


type alias Flags =
    { settings : Settings
    , dataviewTaskCompletion : DataviewTaskCompletion
    , rightToLeft : Bool
    , now : Int
    , zone : Int
    , uniqueId : String
    }


type alias Cursor =
    { x : Float
    , y : Float
    }


type alias DragData =
    { beaconIdentifier : String
    , dragType : String
    , cursor : { x : Float, y : Float }
    , beacons : List BeaconData
    }


type alias BeaconData =
    { id : { identifier : String, position : String }
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias BeaconId =
    { identifier : String
    , position : String
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


trackDraggableEncoder : TsEncode.Encoder { beaconIdentifier : String, clientPos : ( Float, Float ) }
trackDraggableEncoder =
    TsEncode.object
        [ required "beaconIdentifier" .beaconIdentifier TsEncode.string
        , required "clientPos" .clientPos (TsEncode.tuple TsEncode.float TsEncode.float)
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
        |> TsDecode.andMap (TsDecode.field "settings" Settings.decoder)
        |> TsDecode.andMap (TsDecode.field "dataviewTaskCompletion" DataviewTaskCompletion.decoder)
        |> TsDecode.andMap (TsDecode.field "rightToLeft" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "now" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "zone" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "uniqueId" TsDecode.string)


toElm : TsDecode.Decoder ToElm
toElm =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "activeStateUpdated" ActiveStateUpdated TsDecode.bool
        , DecodeHelpers.toElmVariant "configChanged" ConfigChanged configChangedDecoder
        , DecodeHelpers.toElmVariant "elementDragged" ElementDragged elementDraggedDecoder
        , DecodeHelpers.toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , DecodeHelpers.toElmVariant "fileRenamed" FileRenamed renamedFileDecoder
        , DecodeHelpers.toElmVariant "fileUpdated" FileUpdated MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "filterCandidates" FilterCandidates (TsDecode.list Filter.decoder)
        , DecodeHelpers.toElmVariant "allMarkdownLoaded" (always AllMarkdownLoaded) (TsDecode.succeed ())
        , DecodeHelpers.toElmVariant "settingsUpdated" SettingsUpdated Settings.decoder
        , DecodeHelpers.toElmVariant "showBoard" ShowBoard TsDecode.int
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAddFilePreviewHovers vCloseView vDeleteTask vDisplayTaskMarkdown vElmInitialized vOpenTaskSourceFile vRequestPaths vTrackDraggable _ vUpdateTasks value ->
            case value of
                AddFilePreviewHovers info ->
                    vAddFilePreviewHovers info

                CloseView ->
                    vCloseView

                DeleteTask info ->
                    vDeleteTask info

                DisplayTaskMarkdown info ->
                    vDisplayTaskMarkdown info

                ElmInitialized ->
                    vElmInitialized

                OpenTaskSourceFile info ->
                    vOpenTaskSourceFile info

                RequestFilterCandidates ->
                    vRequestPaths

                TrackDraggable info ->
                    vTrackDraggable info

                UpdateTasks info ->
                    vUpdateTasks info
        )
        |> TsEncode.variantTagged "addFilePreviewHovers" addFilePreviewHoversEncoder
        |> TsEncode.variant0 "closeView"
        |> TsEncode.variantTagged "deleteTask" deleteTaskEncoder
        |> TsEncode.variantTagged "displayTaskMarkdown" displayTaskMarkdownEncoder
        |> TsEncode.variant0 "elmInitialized"
        |> TsEncode.variantTagged "openTaskSourceFile" openTaskSourceFileEncoder
        |> TsEncode.variant0 "requestFilterCandidates"
        |> TsEncode.variantTagged "trackDraggable" trackDraggableEncoder
        |> TsEncode.variantTagged "updateSettings" Settings.encoder
        |> TsEncode.variantTagged "updateTasks" updateTasksEncoder
        |> TsEncode.buildUnion



-- HELPERS


configChangedDecoder : TsDecode.Decoder TextDirection
configChangedDecoder =
    TsDecode.succeed TextDirection.fromRtlFlag
        |> TsDecode.andMap (TsDecode.field "rightToLeft" TsDecode.bool)


elementDraggedDecoder : TsDecode.Decoder DragData
elementDraggedDecoder =
    TsDecode.succeed DragData
        |> TsDecode.andMap (TsDecode.field "beaconIdentifier" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "dragType" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "cursor" cursorDecoder)
        |> TsDecode.andMap (TsDecode.field "beacons" (TsDecode.list beaconDataDecoder))


cursorDecoder : TsDecode.Decoder Cursor
cursorDecoder =
    TsDecode.succeed Cursor
        |> TsDecode.andMap (TsDecode.field "x" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "y" TsDecode.float)


beaconDataDecoder : TsDecode.Decoder BeaconData
beaconDataDecoder =
    TsDecode.succeed BeaconData
        |> TsDecode.andMap (TsDecode.field "id" beaconIdDecoder)
        |> TsDecode.andMap (TsDecode.field "x" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "y" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "width" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "height" TsDecode.float)


beaconIdDecoder : TsDecode.Decoder BeaconId
beaconIdDecoder =
    TsDecode.succeed BeaconId
        |> TsDecode.andMap (TsDecode.field "identifier" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "position" TsDecode.string)


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
