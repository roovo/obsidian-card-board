module DateBoardConfig exposing
    ( DateBoardConfig
    , collapseColumn
    , completedCount
    , decoder_v_0_10_0
    , decoder_v_0_11_0
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , decoder_v_0_6_0
    , decoder_v_0_7_0
    , decoder_v_0_8_0
    , decoder_v_0_9_0
    , default
    , displayUndated
    , encoder
    , setNamesToDefault
    , tagsToHide
    , toggleIncludeUndated
    , updateCompletedCount
    )

import CollapsedColumns exposing (CollapsedColumns)
import Column exposing (Column)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.Dated as DatedColumn exposing (DatedColumn)
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
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


decoder_v_0_11_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_11_0 =
    TsDecode.succeed DateBoardConfig
        |> TsDecode.required "columns" Columns.decoder
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_10_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_10_0 =
    decoder_v_0_9_0


decoder_v_0_9_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_9_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.required "collapsedColumns" (TsDecode.list TsDecode.int)
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_8_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_8_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.hardcoded []
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.required "filters" (TsDecode.list Filter.decoder)
        |> TsDecode.required "filterPolarity" Filter.polarityDecoder
        |> TsDecode.required "filterScope" Filter.scopeDecoder
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.required "showFilteredTags" TsDecode.bool
        |> TsDecode.required "title" TsDecode.string


decoder_v_0_7_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_7_0 =
    decoder_v_0_6_0


decoder_v_0_6_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_6_0 =
    decoder_v_0_5_0


decoder_v_0_5_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_5_0 =
    decoder_v_0_4_0


decoder_v_0_4_0 : TsDecode.Decoder DateBoardConfig
decoder_v_0_4_0 =
    TsDecode.succeed buildfromPreV11
        |> TsDecode.hardcoded []
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
        |> TsDecode.required "completedCount" TsDecode.int
        |> TsDecode.hardcoded []
        |> TsDecode.hardcoded Filter.Allow
        |> TsDecode.hardcoded Filter.Both
        |> TsDecode.required "includeUndated" TsDecode.bool
        |> TsDecode.hardcoded True
        |> TsDecode.required "title" TsDecode.string



-- INFO


completedCount : DateBoardConfig -> Int
completedCount dateBoardConfig =
    Columns.completedCount dateBoardConfig.columns


displayUndated : DateBoardConfig -> Bool
displayUndated dateBoardConfig =
    Columns.includesUndated dateBoardConfig.columns


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


collapseColumn : Int -> Bool -> DateBoardConfig -> DateBoardConfig
collapseColumn columnIndex isCollapsed dateBoardConfig =
    { dateBoardConfig | columns = Columns.collapseColumn columnIndex isCollapsed dateBoardConfig.columns }


toggleIncludeUndated : DateBoardConfig -> DateBoardConfig
toggleIncludeUndated dateBoardConfig =
    { dateBoardConfig | columns = Columns.toggleIncludeUndated dateBoardConfig.columns }


setNamesToDefault : DefaultColumnNames -> DateBoardConfig -> DateBoardConfig
setNamesToDefault defaultColumnNames dateBoardConfig =
    { dateBoardConfig | columns = Columns.setNamesToDefault defaultColumnNames dateBoardConfig.columns }


updateCompletedCount : Int -> DateBoardConfig -> DateBoardConfig
updateCompletedCount newCount dateBoardConfig =
    { dateBoardConfig | columns = Columns.updateCompletedCount newCount dateBoardConfig.columns }



-- PRIVATE


buildfromPreV11 : List Int -> Int -> List Filter -> Polarity -> Scope -> Bool -> Bool -> String -> DateBoardConfig
buildfromPreV11 collapsedColumns completedCount_ filters filterPolarity filterScope includeUndated_ showFilteredTags title =
    let
        columns =
            undatedColumn
                ++ datedColumns
                ++ completedColumn
                |> List.indexedMap handleCollapse

        completedColumn =
            if completedCount_ > 0 then
                [ Column.completed <| CompletedColumn.init "Completed" completedColumnIndex completedCount_ ]

            else
                []

        completedColumnIndex =
            List.length (undatedColumn ++ datedColumns)

        datedColumns =
            [ Column.dated <| DatedColumn.init "Today" (DatedColumn.Before 1)
            , Column.dated <| DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 1, to = 1 })
            , Column.dated <| DatedColumn.init "Future" (DatedColumn.After 1)
            ]

        handleCollapse : Int -> Column -> Column
        handleCollapse index column =
            if List.member index collapsedColumns then
                Column.toggleCollapse column

            else
                column

        undatedColumn =
            if includeUndated_ then
                [ Column.undated "Undated" ]

            else
                []
    in
    { columns = Columns.fromList columns
    , filters = filters
    , filterPolarity = filterPolarity
    , filterScope = filterScope
    , showFilteredTags = showFilteredTags
    , title = title
    }
