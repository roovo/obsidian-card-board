module GlobalSettings exposing
    ( GlobalSettings
    , TaskCompletionFormat(..)
    , decoder
    , default
    , encoder
    , updateTaskUpdateFormat
    )

import Json.Encode as JE
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type TaskCompletionFormat
    = ObsidianCardBoard
    | ObsidianDataview
    | ObsidianTasks


type alias GlobalSettings =
    { taskCompletionFormat : TaskCompletionFormat
    }


default : GlobalSettings
default =
    { taskCompletionFormat = ObsidianCardBoard
    }



-- UTILITIES


updateTaskUpdateFormat : String -> GlobalSettings -> GlobalSettings
updateTaskUpdateFormat taskCompletionFormat gs =
    { gs | taskCompletionFormat = taskUpdateFormatFromString taskCompletionFormat }



-- SERIALISE


encoder : TsEncode.Encoder GlobalSettings
encoder =
    TsEncode.object
        [ TsEncode.required "taskCompletionFormat" .taskCompletionFormat taskUpdateFormatEncoder
        ]


decoder : TsDecode.Decoder GlobalSettings
decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskUpdateFormatDecoder)



-- PRIVATE


taskUpdateFormatDecoder : TsDecode.Decoder TaskCompletionFormat
taskUpdateFormatDecoder =
    TsDecode.oneOf
        [ TsDecode.literal ObsidianCardBoard (JE.string "ObsidianCardBoard")
        , TsDecode.literal ObsidianDataview (JE.string "ObsidianDataview")
        , TsDecode.literal ObsidianTasks (JE.string "ObsidianTasks")
        ]


taskUpdateFormatEncoder : TsEncode.Encoder TaskCompletionFormat
taskUpdateFormatEncoder =
    TsEncode.union
        (\vObsidianCardBoard vObsidianDataview vObsidianTasks v ->
            case v of
                ObsidianCardBoard ->
                    vObsidianCardBoard

                ObsidianDataview ->
                    vObsidianDataview

                ObsidianTasks ->
                    vObsidianTasks
        )
        |> TsEncode.variantLiteral (JE.string "ObsidianCardBoard")
        |> TsEncode.variantLiteral (JE.string "ObsidianDataview")
        |> TsEncode.variantLiteral (JE.string "ObsidianTasks")
        |> TsEncode.buildUnion


taskUpdateFormatFromString : String -> TaskCompletionFormat
taskUpdateFormatFromString source =
    if source == "ObsidianDataview" then
        ObsidianDataview

    else if source == "ObsidianTasks" then
        ObsidianTasks

    else
        ObsidianCardBoard
