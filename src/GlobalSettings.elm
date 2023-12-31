module GlobalSettings exposing
    ( GlobalSettings
    , TaskCompletionFormat(..)
    , default
    , encoder
    , v_0_10_0_decoder
    , v_0_11_0_decoder
    , v_0_5_0_decoder
    , v_0_6_0_decoder
    , v_0_7_0_decoder
    , v_0_8_0_decoder
    , v_0_9_0_decoder
    )

import DefaultColumnNames exposing (DefaultColumnNames)
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
    { defaultColumnNames : DefaultColumnNames
    , ignoreFileNameDates : Bool
    , taskCompletionFormat : TaskCompletionFormat
    , taskCompletionInLocalTime : Bool
    , taskCompletionWithUtcOffset : Bool
    }


default : GlobalSettings
default =
    { defaultColumnNames = DefaultColumnNames.default
    , ignoreFileNameDates = False
    , taskCompletionFormat = ObsidianCardBoard
    , taskCompletionInLocalTime = True
    , taskCompletionWithUtcOffset = True
    }



-- SERIALISE


encoder : TsEncode.Encoder GlobalSettings
encoder =
    TsEncode.object
        [ TsEncode.required "defaultColumnNames" .defaultColumnNames DefaultColumnNames.encoder
        , TsEncode.required "ignoreFileNameDates" .ignoreFileNameDates TsEncode.bool
        , TsEncode.required "taskCompletionFormat" .taskCompletionFormat taskCompletionFormatEncoder
        , TsEncode.required "taskCompletionInLocalTime" .ignoreFileNameDates TsEncode.bool
        , TsEncode.required "taskCompletionWithUtcOffset" .ignoreFileNameDates TsEncode.bool
        ]


v_0_12_0_decoder : TsDecode.Decoder GlobalSettings
v_0_12_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "defaultColumnNames" DefaultColumnNames.v_0_11_0_decoder)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.field "taskCompletionInLocalTime" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionWithUtcOffset" TsDecode.bool)


v_0_11_0_decoder : TsDecode.Decoder GlobalSettings
v_0_11_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "defaultColumnNames" DefaultColumnNames.v_0_11_0_decoder)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_10_0_decoder : TsDecode.Decoder GlobalSettings
v_0_10_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_10_0_decoder)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_9_0_decoder : TsDecode.Decoder GlobalSettings
v_0_9_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_9_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_8_0_decoder : TsDecode.Decoder GlobalSettings
v_0_8_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_8_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_7_0_decoder : TsDecode.Decoder GlobalSettings
v_0_7_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_7_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_6_0_decoder : TsDecode.Decoder GlobalSettings
v_0_6_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.succeed DefaultColumnNames.default)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_5_0_decoder : TsDecode.Decoder GlobalSettings
v_0_5_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.succeed DefaultColumnNames.default)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskUpdateFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)



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
