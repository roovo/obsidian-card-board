module GlobalSettings exposing
    ( FirstDayOfWeek(..)
    , GlobalSettings
    , TaskCompletionFormat(..)
    , TaskCompletionSettings
    , default
    , encoder
    , taskCompletionSettings
    , v_0_10_0_decoder
    , v_0_11_0_decoder
    , v_0_12_0_decoder
    , v_0_13_0_decoder
    , v_0_5_0_decoder
    , v_0_6_0_decoder
    , v_0_7_0_decoder
    , v_0_8_0_decoder
    , v_0_9_0_decoder
    )

import DefaultColumnNames exposing (DefaultColumnNames)
import Filter exposing (Filter)
import Json.Encode as JE
import Time
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type FirstDayOfWeek
    = FromLocale
    | SpecificWeekday Time.Weekday


type TaskCompletionFormat
    = NoCompletion
    | ObsidianCardBoard
    | ObsidianDataview
    | ObsidianTasks


type alias GlobalSettings =
    { defaultColumnNames : DefaultColumnNames
    , filters : List Filter
    , firstDayOfWeek : FirstDayOfWeek
    , ignoreFileNameDates : Bool
    , taskCompletionFormat : TaskCompletionFormat
    , taskCompletionInLocalTime : Bool
    , taskCompletionShowUtcOffset : Bool
    }


type alias TaskCompletionSettings =
    { format : TaskCompletionFormat
    , inLocalTime : Bool
    , showUtcOffset : Bool
    }


default : GlobalSettings
default =
    { defaultColumnNames = DefaultColumnNames.default
    , filters = []
    , firstDayOfWeek = FromLocale
    , ignoreFileNameDates = False
    , taskCompletionFormat = ObsidianCardBoard
    , taskCompletionInLocalTime = True
    , taskCompletionShowUtcOffset = True
    }



-- SERIALISE


encoder : TsEncode.Encoder GlobalSettings
encoder =
    TsEncode.object
        [ TsEncode.required "defaultColumnNames" .defaultColumnNames DefaultColumnNames.encoder
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "firstDayOfWeek" .firstDayOfWeek firstDayOfWeekEncoder
        , TsEncode.required "ignoreFileNameDates" .ignoreFileNameDates TsEncode.bool
        , TsEncode.required "taskCompletionFormat" .taskCompletionFormat taskCompletionFormatEncoder
        , TsEncode.required "taskCompletionInLocalTime" .taskCompletionInLocalTime TsEncode.bool
        , TsEncode.required "taskCompletionShowUtcOffset" .taskCompletionShowUtcOffset TsEncode.bool
        ]


v_0_13_0_decoder : TsDecode.Decoder GlobalSettings
v_0_13_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "defaultColumnNames" DefaultColumnNames.v_0_13_0_decoder)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "firstDayOfWeek" firstDayOfWeekDecoder)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.field "taskCompletionInLocalTime" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionShowUtcOffset" TsDecode.bool)



-- INFO


taskCompletionSettings : GlobalSettings -> TaskCompletionSettings
taskCompletionSettings globalSettings =
    { format = globalSettings.taskCompletionFormat
    , inLocalTime = globalSettings.taskCompletionInLocalTime
    , showUtcOffset = globalSettings.taskCompletionShowUtcOffset
    }



-- LEGACY


v_0_12_0_decoder : TsDecode.Decoder GlobalSettings
v_0_12_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "defaultColumnNames" DefaultColumnNames.v_0_12_0_decoder)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.field "taskCompletionInLocalTime" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionShowUtcOffset" TsDecode.bool)


v_0_11_0_decoder : TsDecode.Decoder GlobalSettings
v_0_11_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "defaultColumnNames" DefaultColumnNames.v_0_11_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_10_0_decoder : TsDecode.Decoder GlobalSettings
v_0_10_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_10_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.field "ignoreFileNameDates" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_9_0_decoder : TsDecode.Decoder GlobalSettings
v_0_9_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_9_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_8_0_decoder : TsDecode.Decoder GlobalSettings
v_0_8_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_8_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_7_0_decoder : TsDecode.Decoder GlobalSettings
v_0_7_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "columnNames" DefaultColumnNames.v_0_7_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_6_0_decoder : TsDecode.Decoder GlobalSettings
v_0_6_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.succeed DefaultColumnNames.default)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskCompletionFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)


v_0_5_0_decoder : TsDecode.Decoder GlobalSettings
v_0_5_0_decoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.succeed DefaultColumnNames.default)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed FromLocale)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "taskUpdateFormat" taskCompletionFormatDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.succeed False)



-- PRIVATE


firstDayOfWeekDecoder : TsDecode.Decoder FirstDayOfWeek
firstDayOfWeekDecoder =
    TsDecode.oneOf
        [ TsDecode.literal FromLocale (JE.string "FromLocale")
        , TsDecode.literal (SpecificWeekday Time.Mon) (JE.string "Mon")
        , TsDecode.literal (SpecificWeekday Time.Tue) (JE.string "Tue")
        , TsDecode.literal (SpecificWeekday Time.Wed) (JE.string "Wed")
        , TsDecode.literal (SpecificWeekday Time.Thu) (JE.string "Thu")
        , TsDecode.literal (SpecificWeekday Time.Fri) (JE.string "Fri")
        , TsDecode.literal (SpecificWeekday Time.Sat) (JE.string "Sat")
        , TsDecode.literal (SpecificWeekday Time.Sun) (JE.string "Sun")
        ]


firstDayOfWeekEncoder : TsEncode.Encoder FirstDayOfWeek
firstDayOfWeekEncoder =
    TsEncode.union
        (\vFromLocale vMon vTue vWed vThu vFri vSat vSun v ->
            case v of
                FromLocale ->
                    vFromLocale

                SpecificWeekday weekday ->
                    case weekday of
                        Time.Mon ->
                            vMon

                        Time.Tue ->
                            vTue

                        Time.Wed ->
                            vWed

                        Time.Thu ->
                            vThu

                        Time.Fri ->
                            vFri

                        Time.Sat ->
                            vSat

                        Time.Sun ->
                            vSun
        )
        |> TsEncode.variantLiteral (JE.string "FromLocale")
        |> TsEncode.variantLiteral (JE.string "Mon")
        |> TsEncode.variantLiteral (JE.string "Tue")
        |> TsEncode.variantLiteral (JE.string "Wed")
        |> TsEncode.variantLiteral (JE.string "Thu")
        |> TsEncode.variantLiteral (JE.string "Fri")
        |> TsEncode.variantLiteral (JE.string "Sat")
        |> TsEncode.variantLiteral (JE.string "Sun")
        |> TsEncode.buildUnion


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
