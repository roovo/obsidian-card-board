module Worker.InteropDefinitions exposing
    ( Flags
    , FromElm(..)
    , ToElm(..)
    , interop
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import DecodeHelpers
import MarkdownFile exposing (MarkdownFile)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


type FromElm
    = AllTasksLoaded
    | AllTaskItems TaskList
    | TasksAdded TaskList
    | TasksDeleted (List TaskItem)
    | TasksDeletedAndAdded ( List String, List TaskItem )
    | TasksUpdated (List ( String, TaskItem ))


type ToElm
    = AllMarkdownLoaded
    | ViewInitialized
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
        , DecodeHelpers.toElmVariant0 "viewInitialized" ViewInitialized
        , DecodeHelpers.toElmVariant "fileAdded" FileAdded MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileDeleted" FileDeleted TsDecode.string
        , DecodeHelpers.toElmVariant "fileModified" FileModified MarkdownFile.decoder
        , DecodeHelpers.toElmVariant "fileRenamed" FileRenamed renamedFileDecoder
        ]


fromElm : TsEncode.Encoder FromElm
fromElm =
    TsEncode.union
        (\vAllTasksLoaded vAllTaskItems vTasksAdded vTasksDeleted vTasksDeletedAndAdded vTasksUpdated value ->
            case value of
                AllTasksLoaded ->
                    vAllTasksLoaded

                AllTaskItems taskList ->
                    vAllTaskItems taskList

                TasksAdded taskList ->
                    vTasksAdded taskList

                TasksDeleted taskItems ->
                    vTasksDeleted taskItems

                TasksDeletedAndAdded toDeletedAndAdd ->
                    vTasksDeletedAndAdded toDeletedAndAdd

                TasksUpdated tasksToUpdate ->
                    vTasksUpdated tasksToUpdate
        )
        |> TsEncode.variant0 "allTasksLoaded"
        |> TsEncode.variantTagged "allTaskItems" TaskList.encoder
        |> TsEncode.variantTagged "tasksAdded" TaskList.encoder
        |> TsEncode.variantTagged "tasksDeleted" tasksDeletedEncoder
        |> TsEncode.variantTagged "tasksDeletedAndAdded" tasksDeletedAndAddedEncoder
        |> TsEncode.variantTagged "tasksUpdated" tasksUpdatedEncoder
        |> TsEncode.buildUnion



-- HELPERS


renamedFileDecoder : TsDecode.Decoder ( String, String )
renamedFileDecoder =
    TsDecode.succeed Tuple.pair
        |> TsDecode.andMap (TsDecode.field "oldPath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "newPath" TsDecode.string)


tasksDeletedAndAddedEncoder : TsEncode.Encoder ( List String, List TaskItem )
tasksDeletedAndAddedEncoder =
    TsEncode.tuple (TsEncode.list TsEncode.string) (TsEncode.list TaskItem.encoder)


tasksDeletedEncoder : TsEncode.Encoder (List TaskItem)
tasksDeletedEncoder =
    TsEncode.list <| TsEncode.map TaskItem.id TsEncode.string


tasksUpdatedEncoder : TsEncode.Encoder (List ( String, TaskItem ))
tasksUpdatedEncoder =
    TsEncode.list <| TsEncode.tuple TsEncode.string TaskItem.encoder
