port module Worker.InteropPorts exposing
    ( allTaskItems
    , allTasksLoaded
    , decodeFlags
    , tasksAdded
    , tasksDeleted
    , tasksDeletedAndAdded
    , tasksUpdated
    , toElm
    )

import Json.Decode
import Json.Encode
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode
import Worker.InteropDefinitions as InteropDefinitions


toElm : Sub (Result Json.Decode.Error InteropDefinitions.ToElm)
toElm =
    (InteropDefinitions.interop.toElm |> TsDecode.decoder)
        |> Json.Decode.decodeValue
        |> interopToElm



-- FLAGS


decodeFlags : Json.Decode.Value -> Result Json.Decode.Error InteropDefinitions.Flags
decodeFlags flags =
    Json.Decode.decodeValue
        (InteropDefinitions.interop.flags |> TsDecode.decoder)
        flags



-- COMMANDS


allTaskItems : TaskList -> Cmd msg
allTaskItems taskList =
    taskList
        |> InteropDefinitions.AllTaskItems
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


allTasksLoaded : Cmd msg
allTasksLoaded =
    InteropDefinitions.AllTasksLoaded
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


tasksAdded : TaskList -> Cmd msg
tasksAdded taskList =
    taskList
        |> InteropDefinitions.TasksAdded
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


tasksDeleted : List TaskItem -> Cmd msg
tasksDeleted taskItems =
    taskItems
        |> InteropDefinitions.TasksDeleted
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


tasksDeletedAndAdded : List String -> List TaskItem -> Cmd msg
tasksDeletedAndAdded toDelete toAdd =
    ( toDelete, toAdd )
        |> InteropDefinitions.TasksDeletedAndAdded
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm


tasksUpdated : List ( String, TaskItem ) -> Cmd msg
tasksUpdated updateDetails =
    updateDetails
        |> InteropDefinitions.TasksUpdated
        |> TsEncode.encoder InteropDefinitions.interop.fromElm
        |> interopFromElm



-- PORTS


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg
