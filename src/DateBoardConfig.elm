module DateBoardConfig exposing
    ( DateBoardConfig
    , decoder_v_0_10_0
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , decoder_v_0_9_0
    , default
    , encoder
    )

import CollapsedColumns exposing (CollapsedColumns)
import ColumnConfig exposing (ColumnConfig)
import Filter exposing (Filter, Polarity, Scope)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias DateBoardConfig =
    { collapsedColumns : CollapsedColumns
    , columnConfigs : List ColumnConfig
    , completedCount : Int
    , filters : List Filter
    , filterPolarity : Polarity
    , filterScope : Scope
    , includeUndated : Bool
    , showFilteredTags : Bool
    , title : String
    }


default : DateBoardConfig
default =
    { completedCount = 10
    , columnConfigs = []
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , filterScope = Filter.defaultScope
    , showFilteredTags = True
    , includeUndated = True
    , title = ""
    , collapsedColumns = CollapsedColumns.init
    }



-- SERIALIZE


encoder : TsEncode.Encoder DateBoardConfig
encoder =
    TsEncode.object
        [ TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "filterScope" .filterScope Filter.scopeEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "includeUndated" .includeUndated TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        , TsEncode.required "collapsedColumns" .collapsedColumns CollapsedColumns.encoder
        ]


decoder_v_0_10_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_10_0 =
    decoder_v_0_9_0


decoder_v_0_9_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_9_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.field "collapsedColumns" CollapsedColumns.decoder)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "filterScope" <| Filter.scopeDecoder)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_5_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_5_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "filterScope" <| Filter.scopeDecoder)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_4_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_4_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_3_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_3_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_2_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_2_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


decoder_v_0_1_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_1_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.andMap (TsDecode.succeed CollapsedColumns.init)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)
