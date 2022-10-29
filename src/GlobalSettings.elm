module GlobalSettings exposing
    ( GlobalSettings
    , TaskUpdateFormat(..)
    , decoder
    , default
    , encoder
    , updateTaskUpdateFormat
    )

import Json.Encode as JE
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type TaskUpdateFormat
    = ObsidianCardBoard
    | ObsidianDataview
    | ObsidianTasks


type alias GlobalSettings =
    { taskUpdateFormat : TaskUpdateFormat
    }


default : GlobalSettings
default =
    { taskUpdateFormat = ObsidianCardBoard
    }



-- UTILITIES


updateTaskUpdateFormat : String -> GlobalSettings -> GlobalSettings
updateTaskUpdateFormat taskUpdateFormat gs =
    { gs | taskUpdateFormat = taskUpdateFormatFromString taskUpdateFormat }



-- SERIALISE


encoder : TsEncode.Encoder GlobalSettings
encoder =
    TsEncode.object
        [ TsEncode.required "taskUpdateFormat" .taskUpdateFormat taskUpdateFormatEncoder
        ]


decoder : TsDecode.Decoder GlobalSettings
decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "taskUpdateFormat" taskUpdateFormatDecoder)



-- PRIVATE


taskUpdateFormatDecoder : TsDecode.Decoder TaskUpdateFormat
taskUpdateFormatDecoder =
    TsDecode.oneOf
        [ TsDecode.literal ObsidianCardBoard (JE.string "ObsidianCardBoard")
        , TsDecode.literal ObsidianDataview (JE.string "ObsidianDataview")
        , TsDecode.literal ObsidianTasks (JE.string "ObsidianTasks")
        ]


taskUpdateFormatEncoder : TsEncode.Encoder TaskUpdateFormat
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


taskUpdateFormatFromString : String -> TaskUpdateFormat
taskUpdateFormatFromString source =
    if source == "ObsidianDataview" then
        ObsidianDataview

    else if source == "ObsidianTasks" then
        ObsidianTasks

    else
        ObsidianCardBoard
