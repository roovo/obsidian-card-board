module DateBoardConfig exposing
    ( DateBoardConfig
    , configDecoder_v_0_1_0
    , configDecoder_v_0_2_0
    , configDecoder_v_0_3_0
    , configDecoder_v_0_4_0
    , configEncoder
    , default
    )

import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import Filter exposing (Filter, Polarity(..))
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias DateBoardConfig =
    { completedCount : Int
    , filters : List Filter
    , filterPolarity : Polarity
    , showFilteredTags : Bool
    , includeUndated : Bool
    , title : String
    }


default : DateBoardConfig
default =
    { completedCount = 10
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , showFilteredTags = True
    , includeUndated = True
    , title = ""
    }



-- SERIALIZE


configEncoder : TsEncode.Encoder DateBoardConfig
configEncoder =
    TsEncode.object
        [ TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "includeUndated" .includeUndated TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        ]


configDecoder_v_0_4_0 : TsDecode.Decoder DateBoardConfig
configDecoder_v_0_4_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_3_0 : TsDecode.Decoder DateBoardConfig
configDecoder_v_0_3_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_2_0 : TsDecode.Decoder DateBoardConfig
configDecoder_v_0_2_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed Allow)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_1_0 : TsDecode.Decoder DateBoardConfig
configDecoder_v_0_1_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Allow)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
