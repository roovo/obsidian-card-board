port module Worker.InteropPorts exposing
    ( allTasksLoaded
    , decodeFlags
    , tasksAdded
    , toElm
    )

import Json.Decode
import Json.Encode
import Settings exposing (Settings)
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
    encodeVariant "allTasksLoaded" (TsEncode.object []) ()
        |> interopFromElm


tasksAdded : TaskList -> Cmd msg
tasksAdded taskList =
    taskList
        |> encodeVariant "tasksAdded" TaskList.encoder
        |> interopFromElm



-- PORTS


port interopFromElm : Json.Encode.Value -> Cmd msg


port interopToElm : (Json.Decode.Value -> msg) -> Sub msg



-- HELPERS


encodeVariant : String -> TsEncode.Encoder arg1 -> arg1 -> Json.Encode.Value
encodeVariant variantName encoder_ arg1 =
    arg1
        |> (TsEncode.object
                [ TsEncode.required "tag" identity (TsEncode.literal (Json.Encode.string variantName))
                , TsEncode.required "data" identity encoder_
                ]
                |> TsEncode.encoder
           )
