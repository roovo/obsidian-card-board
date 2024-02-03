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
    | TasksAdded TaskList
    | TasksDeleted (List TaskItem)


type ToElm
    = AllMarkdownLoaded
    | FileAdded MarkdownFile
    | FileDeleted String
    | FileModified MarkdownFile
    | FileRenamed ( String, String )


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
        [ DecodeHelpers.toElmVariant "allMarkdownLoaded" (always AllMarkdownLoaded) (TsDecode.succeed ())
        , DecodeHelpers.toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , DecodeHelpers.toElmVariant "fileModified" FileModified MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileRenamed" FileRenamed renamedFileDecoder
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAllTasksLoaded vTasksAdded vTasksDeleted value ->
            case value of
                AllTasksLoaded ->
                    vAllTasksLoaded

                TasksAdded taskList ->
                    vTasksAdded taskList

                TasksDeleted taskItems ->
                    vTasksDeleted taskItems
        )
        |> TsEncode.variant0 "allTasksLoaded"
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
