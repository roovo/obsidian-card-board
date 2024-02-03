port module Worker.InteropPorts exposing
    ( allTasksLoaded
    , decodeFlags
    , tasksAdded
    , tasksDeleted
    , toElm
    )

import Json.Decode
import Json.Encode
import Settings exposing (Settings)
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



-- COMMANDS


decodeFlags : Json.Decode.Value -> Result Json.Decode.Error InteropDefinitions.Flags
decodeFlags flags =
    Json.Decode.decodeValue
        (InteropDefinitions.interop.flags |> TsDecode.decoder)
        flags


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



-- PORTS


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg
