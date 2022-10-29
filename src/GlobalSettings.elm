module GlobalSettings exposing
    ( GlobalSettings
    , TaskCompletionFormat(..)
    , default
    , encoder
    , updateTaskCompletionFormat
    , v_0_5_0_decoder
    , v_0_6_0_decoder
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


updateTaskCompletionFormat : String -> GlobalSettings -> GlobalSettings
updateTaskCompletionFormat taskCompletionFormat gs =
    { gs | taskCompletionFormat = taskCompletionFormatFromString taskCompletionFormat }



-- SERIALISE


encoder : TsEncode.Encoder GlobalSettings
encoder =
    TsEncode.object
        [ TsEncode.required "taskCompletionFormat" .taskCompletionFormat taskCompletionFormatEncoder
        ]


v_0_6_0_decoder : TsDecode.Decoder GlobalSettings
v_0_6_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)


v_0_5_0_decoder : TsDecode.Decoder GlobalSettings
v_0_5_0_decoder =
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


taskCompletionFormatFromString : String -> TaskCompletionFormat
taskCompletionFormatFromString source =
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
