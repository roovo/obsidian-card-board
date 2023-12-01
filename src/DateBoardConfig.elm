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
    , populateColummConfigs
    , tagsToHide
    )

import CollapsedColumns exposing (CollapsedColumns)
import Column exposing (Column)
import Column.Dated as DatedColumn exposing (DatedColumn)
import ColumnNames exposing (ColumnNames)
import Columns exposing (Columns)
import Filter exposing (Filter, Polarity, Scope)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias DateBoardConfig =
    { collapsedColumns : CollapsedColumns
    , columnConfigs : Columns
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
    { collapsedColumns = CollapsedColumns.init
    , columnConfigs =
        Columns.fromList ColumnNames.default
            [ Column.undated "Undated"
            , Column.todayColumn
            , Column.tomorrowColumn
            , Column.futureColumn
            ]
            10
    , completedCount = 10
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , filterScope = Filter.defaultScope
    , includeUndated = True
    , showFilteredTags = True
    , title = ""
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
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
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
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
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
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
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
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
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
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
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
        |> TsDecode.andMap (TsDecode.succeed Columns.empty)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed Filter.Both)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.succeed True)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)



-- INFO


tagsToHide : DateBoardConfig -> List String
tagsToHide dateBoardConfig =
    if dateBoardConfig.showFilteredTags then
        []

    else
        dateBoardConfig
            |> .filters
            |> List.filter (\f -> Filter.filterType f == "Tags")
            |> List.map Filter.value



-- MODIFICATION


populateColummConfigs : ColumnNames -> DateBoardConfig -> DateBoardConfig
populateColummConfigs columnNames dateBoardConfig =
    let
        dateColumns =
            [ Column.dated <|
                DatedColumn.init
                    (ColumnNames.nameFor "today" columnNames)
                    (DatedColumn.Before 1)
            , Column.dated <|
                DatedColumn.init
                    (ColumnNames.nameFor "tomorrow" columnNames)
                    (DatedColumn.Between 1 1)
            , Column.dated <|
                DatedColumn.init
                    (ColumnNames.nameFor "future" columnNames)
                    (DatedColumn.After 1)
            ]

        undated =
            if dateBoardConfig.includeUndated then
                [ Column.undated (ColumnNames.nameFor "undated" columnNames) ]

            else
                []
    in
    { dateBoardConfig | columnConfigs = Columns.fromList columnNames (undated ++ dateColumns) dateBoardConfig.completedCount }
