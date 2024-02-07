module Worker.InteropDefinitions exposing
    ( Flags
    , FromElm(..)
    , ToElm(..)
    , fromElm
    , interop
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import DecodeHelpers
import DragAndDrop.Coords as Coords exposing (Coords)
import DragAndDrop.DragData as DragData exposing (DragData)
import Filter exposing (Filter)
import MarkdownFile exposing (MarkdownFile)
import Settings exposing (Settings)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TextDirection exposing (TextDirection)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (required)


type FromElm
    = AllTasksLoaded
    | AllTaskItems TaskList
    | TasksAdded TaskList
    | TasksDeleted (List TaskItem)


type ToElm
    = AllMarkdownLoaded
    | BroadcastAllTaskItems
    | FileAdded MarkdownFile
    | FileDeleted String
    | FileModified MarkdownFile
    | FileRenamed ( String, String )


type alias Flags =
    { dataviewTaskCompletion : DataviewTaskCompletion }


interop : { toElm : TsDecode.Decoder ToElm, fromElm : TsEncode.Encoder FromElm, flags : TsDecode.Decoder Flags }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }



-- INTEROP


flags : TsDecode.Decoder Flags
flags =
    TsDecode.succeed Flags
        |> TsDecode.andMap (TsDecode.field "dataviewTaskCompletion" DataviewTaskCompletion.decoder)


toElm : TsDecode.Decoder ToElm
toElm =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "allMarkdownLoaded" (always AllMarkdownLoaded) (TsDecode.succeed ())
        , DecodeHelpers.toElmVariant0 "broadcastAllTaskItems" BroadcastAllTaskItems
        , DecodeHelpers.toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , DecodeHelpers.toElmVariant "fileModified" FileModified MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileRenamed" FileRenamed renamedFileDecoder
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAllTasksLoaded vAllTaskItems vTasksAdded vTasksDeleted value ->
            case value of
                AllTasksLoaded ->
                    vAllTasksLoaded

                AllTaskItems taskList ->
                    vAllTaskItems taskList

                TasksAdded taskList ->
                    vTasksAdded taskList

                TasksDeleted taskItems ->
                    vTasksDeleted taskItems
        )
        |> TsEncode.variant0 "allTasksLoaded"
        |> TsEncode.variantTagged "allTaskItems" TaskList.encoder
        |> TsEncode.variantTagged "tasksAdded" TaskList.encoder
        |> TsEncode.variantTagged "tasksDeleted" tasksDeletedEncoder
        |> TsEncode.buildUnion



-- HELPERS


renamedFileDecoder : TsDecode.Decoder ( String, String )
renamedFileDecoder =
    TsDecode.succeed Tuple.pair
        |> TsDecode.andMap (TsDecode.field "oldPath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "newPath" TsDecode.string)


tasksDeletedEncoder : TsEncode.Encoder (List TaskItem)
tasksDeletedEncoder =
    TsEncode.list <| TsEncode.map TaskItem.id TsEncode.string
