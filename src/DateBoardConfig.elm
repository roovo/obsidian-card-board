module DateBoardConfig exposing
    ( DateBoardConfig
    , completedCount
    , decoder_v_0_10_0
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , decoder_v_0_9_0
    , default
    , encoder
    , includeUndated
    , populateColummConfigs
    , tagsToHide
    )

import CollapsedColumns exposing (CollapsedColumns)
import Column exposing (Column)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.Dated as DatedColumn exposing (DatedColumn)
import ColumnNames exposing (ColumnNames)
import Columns exposing (Columns)
import Filter exposing (Filter, Polarity, Scope)
import TsJson.Decode as TsDecode
import TsJson.Decode.Pipeline as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias DateBoardConfig =
    { columns : Columns
    , filters : List Filter
    , filterPolarity : Polarity
    , filterScope : Scope
    , showFilteredTags : Bool
    , title : String
    }


default : DateBoardConfig
default =
    { columns =
        Columns.fromList
            [ Column.undated "Undated"
            , Column.dated <| DatedColumn.init "Today" (DatedColumn.Before 1)
            , Column.dated <| DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 1, to = 1 })
            , Column.dated <| DatedColumn.init "Future" (DatedColumn.After 1)
            , Column.completed <| CompletedColumn.init "Completed" 4 10
            ]
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , filterScope = Filter.defaultScope
    , showFilteredTags = True
    , title = ""
    }



-- SERIALIZE


encoder : TsEncode.Encoder DateBoardConfig
encoder =
    TsEncode.object
        [ TsEncode.required "columns" .columns Columns.encoder
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "filterScope" .filterScope Filter.scopeEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        ]


decoder_v_0_10_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_10_0 =
    decoder_v_0_9_0


decoder_v_0_9_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_9_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.required "collapsedColumns" (TsDecode.list TsDecode.int)
        |> TsDecode.hardcoded Columns.empty
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_5_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_5_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Columns.empty
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_4_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_4_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Columns.empty
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_3_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_3_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Columns.empty
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_2_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_2_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Columns.empty
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.hardcoded Filter.Allow
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_1_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_1_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Columns.empty
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Filter.Allow
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string


buildfromPreV11 : List Int -> Columns -> Int -> List Filter -> Polarity -> Scope -> Bool -> Bool -> String -> DateBoardConfig
buildfromPreV11 collapsedColumns columns completedCount_ filters filterPolarity filterScope includeUndated_ showFilteredTags title =
    let
        datedColumns =
            [ Column.dated <| DatedColumn.init "Today" (DatedColumn.Before 1)
            , Column.dated <| DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 1, to = 1 })
            , Column.dated <| DatedColumn.init "Future" (DatedColumn.After 1)
            ]

        undatedColumn =
            if includeUndated_ then
                [ Column.undated "Undated" ]

            else
                []

        completedColumn =
            if completedCount_ > 0 then
                [ Column.completed <| CompletedColumn.init "Completed" completedColumnIndex completedCount_ ]

            else
                []

        completedColumnIndex =
            List.length (undatedColumn ++ datedColumns)
    in
    { columns = Columns.fromList (undatedColumn ++ datedColumns ++ completedColumn)
    , filters = filters
    , filterPolarity = filterPolarity
    , filterScope = filterScope
    , showFilteredTags = showFilteredTags
    , title = title
    }



-- INFO


completedCount : DateBoardConfig -> Int
completedCount dateBoardConfig =
    10


includeUndated : DateBoardConfig -> Bool
includeUndated dateBoardConfig =
    True


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
    -- let
    --     dateColumns =
    --         [ Column.dated <|
    --             DatedColumn.init
    --                 (ColumnNames.nameFor "today" columnNames)
    --                 (DatedColumn.Before 1)
    --         , Column.dated <|
    --             DatedColumn.init
    --                 (ColumnNames.nameFor "tomorrow" columnNames)
    --                 (DatedColumn.Between 1 1)
    --         , Column.dated <|
    --             DatedColumn.init
    --                 (ColumnNames.nameFor "future" columnNames)
    --                 (DatedColumn.After 1)
    --         ]
    --     undated =
    --         if dateBoardConfig.includeUndated then
    --             [ Column.undated (ColumnNames.nameFor "undated" columnNames) ]
    --         else
    --             []
    -- in
    -- { dateBoardConfig | columnConfigs = Columns.fromList columnNames (undated ++ dateColumns) dateBoardConfig.completedCount }
    dateBoardConfig
