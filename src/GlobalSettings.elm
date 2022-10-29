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
    = NoCompletion
    | ObsidianCardBoard
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
        [ TsEncode.required "taskUpdateFormat" .taskCompletionFormat taskCompletionFormatEncoder
        ]


decoder : TsDecode.Decoder GlobalSettings
decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "taskUpdateFormat" taskCompletionFormatDecoder)



-- PRIVATE


taskCompletionFormatDecoder : TsDecode.Decoder TaskCompletionFormat
taskCompletionFormatDecoder =
    TsDecode.oneOf
        [ TsDecode.literal NoCompletion (JE.string "NoCompletion")
        , TsDecode.literal ObsidianCardBoard (JE.string "ObsidianCardBoard")
        , TsDecode.literal ObsidianDataview (JE.string "ObsidianDataview")
        , TsDecode.literal ObsidianTasks (JE.string "ObsidianTasks")
        ]


taskCompletionFormatEncoder : TsEncode.Encoder TaskCompletionFormat
taskCompletionFormatEncoder =
    TsEncode.union
        (\vNoCompletion vObsidianCardBoard vObsidianDataview vObsidianTasks v ->
            case v of
                NoCompletion ->
                    vNoCompletion

                ObsidianCardBoard ->
                    vObsidianCardBoard

                ObsidianDataview ->
                    vObsidianDataview

                ObsidianTasks ->
                    vObsidianTasks
        )
        |> TsEncode.variantLiteral (JE.string "NoCompletion")
        |> TsEncode.variantLiteral (JE.string "ObsidianCardBoard")
        |> TsEncode.variantLiteral (JE.string "ObsidianDataview")
        |> TsEncode.variantLiteral (JE.string "ObsidianTasks")
        |> TsEncode.buildUnion


taskUpdateFormatFromString : String -> TaskCompletionFormat
taskUpdateFormatFromString source =
    if source == "ObsidianCardBoard" then
        ObsidianCardBoard

    else if source == "ObsidianTasks" then
        ObsidianTasks

    else if source == "ObsidianDataview" then
        ObsidianDataview

    else if source == "NoCompletion" then
        NoCompletion

    else
        ObsidianCardBoard
