module BoardConfig exposing
    ( BoardConfig(..)
    , collapseColumn
    , collapsedColumns
    , decoder_v_0_10_0
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , decoder_v_0_5_0
    , decoder_v_0_6_0
    , decoder_v_0_9_0
    , default
    , encoder
    , filterPolarity
    , filterScope
    , filters
    , fromBoardType
    , isForDateBoard
    , isForTagBoard
    , mapFilters
    , title
    , toggleIncludeOthers
    , toggleIncludeUndated
    , toggleIncludeUntagged
    , toggleShowColumnTags
    , toggleShowFilteredTags
    , toggleTagFilterScope
    , updateBoardType
    , updateColumnNames
    , updateCompletedCount
    , updateFilterPolarity
    , updateFilters
    , updateTags
    , updateTitle
    )

import CollapsedColumns exposing (CollapsedColumns)
import ColumnNames exposing (ColumnNames)
import DateBoardConfig exposing (DateBoardConfig)
import DecodeHelpers
import Filter exposing (Filter, Polarity, Scope)
import Parser
import TagBoardConfig exposing (TagBoardConfig)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type BoardConfig
    = DateBoardConfig DateBoardConfig
    | TagBoardConfig TagBoardConfig



-- CREATE


default : BoardConfig
default =
    TagBoardConfig TagBoardConfig.default


fromBoardType : String -> String -> BoardConfig
fromBoardType boardType title_ =
    case boardType of
        "dateBoard" ->
            let
                newBoardConfig : DateBoardConfig
                newBoardConfig =
                    DateBoardConfig.default
            in
            DateBoardConfig { newBoardConfig | title = title_ }

        _ ->
            let
                newBoardConfig : TagBoardConfig
                newBoardConfig =
                    TagBoardConfig.default
            in
            TagBoardConfig { newBoardConfig | title = title_ }



-- UTILITIES


collapsedColumns : BoardConfig -> CollapsedColumns
collapsedColumns config =
    -- case config of
    --     DateBoardConfig c ->
    --         c.collapsedColumns
    --     TagBoardConfig c ->
    --         c.collapsedColumns
    CollapsedColumns.init


isForDateBoard : BoardConfig -> Bool
isForDateBoard config =
    case config of
        DateBoardConfig _ ->
            True

        _ ->
            False


isForTagBoard : BoardConfig -> Bool
isForTagBoard config =
    case config of
        TagBoardConfig _ ->
            True

        _ ->
            False


filters : BoardConfig -> List Filter
filters config =
    case config of
        DateBoardConfig boardConfig ->
            boardConfig.filters

        TagBoardConfig boardConfig ->
            boardConfig.filters


filterPolarity : BoardConfig -> Polarity
filterPolarity config =
    case config of
        DateBoardConfig boardConfig ->
            boardConfig.filterPolarity

        TagBoardConfig boardConfig ->
            boardConfig.filterPolarity


filterScope : BoardConfig -> Scope
filterScope config =
    case config of
        DateBoardConfig boardConfig ->
            boardConfig.filterScope

        TagBoardConfig boardConfig ->
            boardConfig.filterScope


title : BoardConfig -> String
title config =
    case config of
        DateBoardConfig boardConfig ->
            boardConfig.title

        TagBoardConfig boardConfig ->
            boardConfig.title



-- SERIALIZE


encoder : TsEncode.Encoder BoardConfig
encoder =
    TsEncode.union
        (\vDateBoardConfig vTagBoardConfig value ->
            case value of
                DateBoardConfig config ->
                    vDateBoardConfig config

                TagBoardConfig config ->
                    vTagBoardConfig config
        )
        |> TsEncode.variantTagged "dateBoardConfig" DateBoardConfig.encoder
        |> TsEncode.variantTagged "tagBoardConfig" TagBoardConfig.encoder
        |> TsEncode.buildUnion


decoder_v_0_10_0 : TsDecode.Decoder BoardConfig
decoder_v_0_10_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoardConfig.decoder_v_0_10_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoardConfig.decoder_v_0_10_0
        ]


decoder_v_0_9_0 : TsDecode.Decoder BoardConfig
decoder_v_0_9_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoardConfig.decoder_v_0_9_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoardConfig.decoder_v_0_9_0
        ]


decoder_v_0_6_0 : TsDecode.Decoder BoardConfig
decoder_v_0_6_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoardConfig.decoder_v_0_5_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoardConfig.decoder_v_0_5_0
        ]


decoder_v_0_5_0 : TsDecode.Decoder BoardConfig
decoder_v_0_5_0 =
    decoder_v_0_4_0


decoder_v_0_4_0 : TsDecode.Decoder BoardConfig
decoder_v_0_4_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoardConfig.decoder_v_0_4_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoardConfig.decoder_v_0_4_0
        ]


decoder_v_0_3_0 : TsDecode.Decoder BoardConfig
decoder_v_0_3_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoardConfig.decoder_v_0_3_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoardConfig.decoder_v_0_3_0
        ]


decoder_v_0_2_0 : TsDecode.Decoder BoardConfig
decoder_v_0_2_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoardConfig.decoder_v_0_2_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoardConfig.decoder_v_0_2_0
        ]


decoder_v_0_1_0 : TsDecode.Decoder BoardConfig
decoder_v_0_1_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoardConfig.decoder_v_0_1_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoardConfig.decoder_v_0_1_0
        ]



-- TRANSFORM


mapFilters : (Filter -> Filter) -> BoardConfig -> BoardConfig
mapFilters fn config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | filters = List.map fn boardConfig.filters }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | filters = List.map fn boardConfig.filters }


collapseColumn : Int -> Bool -> BoardConfig -> BoardConfig
collapseColumn columnIndex isCollapsed config =
    -- case config of
    --     DateBoardConfig boardConfig ->
    --         DateBoardConfig <|
    --             { boardConfig | collapsedColumns = CollapsedColumns.collapseColumn columnIndex isCollapsed boardConfig.collapsedColumns }
    --     TagBoardConfig boardConfig ->
    --         TagBoardConfig <|
    --             { boardConfig | collapsedColumns = CollapsedColumns.collapseColumn columnIndex isCollapsed boardConfig.collapsedColumns }
    config


updateColumnNames : ColumnNames -> BoardConfig -> BoardConfig
updateColumnNames columnNames boardConfig =
    case boardConfig of
        DateBoardConfig dateBoardConfig ->
            DateBoardConfig <| DateBoardConfig.updateColumnNames columnNames dateBoardConfig

        TagBoardConfig tagBoardConfig ->
            TagBoardConfig <| TagBoardConfig.updateColumnNames columnNames tagBoardConfig


toggleIncludeOthers : BoardConfig -> BoardConfig
toggleIncludeOthers config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | includeOthers = not boardConfig.includeOthers }


toggleIncludeUndated : BoardConfig -> BoardConfig
toggleIncludeUndated config =
    -- case config of
    --     DateBoardConfig boardConfig ->
    --         DateBoardConfig { boardConfig | includeUndated = not boardConfig.includeUndated }
    --     TagBoardConfig _ ->
    config


toggleIncludeUntagged : BoardConfig -> BoardConfig
toggleIncludeUntagged config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | includeUntagged = not boardConfig.includeUntagged }


toggleShowColumnTags : BoardConfig -> BoardConfig
toggleShowColumnTags config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | showColumnTags = not boardConfig.showColumnTags }


toggleShowFilteredTags : BoardConfig -> BoardConfig
toggleShowFilteredTags config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | showFilteredTags = not boardConfig.showFilteredTags }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | showFilteredTags = not boardConfig.showFilteredTags }


toggleTagFilterScope : BoardConfig -> BoardConfig
toggleTagFilterScope config =
    let
        cycleScope : Scope -> Scope
        cycleScope scope =
            case scope of
                Filter.TopLevelOnly ->
                    Filter.SubTasksOnly

                Filter.SubTasksOnly ->
                    Filter.Both

                Filter.Both ->
                    Filter.TopLevelOnly
    in
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | filterScope = cycleScope boardConfig.filterScope }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | filterScope = cycleScope boardConfig.filterScope }


updateBoardType : String -> BoardConfig -> BoardConfig
updateBoardType boardType config =
    fromBoardType boardType (title config)


updateCompletedCount : Maybe Int -> BoardConfig -> BoardConfig
updateCompletedCount value config =
    -- case ( config, value ) of
    --     ( DateBoardConfig boardConfig, Just newCount ) ->
    --         DateBoardConfig { boardConfig | completedCount = newCount }
    --     ( TagBoardConfig boardConfig, Just newCount ) ->
    --         TagBoardConfig { boardConfig | completedCount = newCount }
    --     _ ->
    config


updateFilterPolarity : String -> BoardConfig -> BoardConfig
updateFilterPolarity polarity config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | filterPolarity = Filter.polarityFromString polarity }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | filterPolarity = Filter.polarityFromString polarity }


updateFilters : List Filter -> BoardConfig -> BoardConfig
updateFilters filters_ config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | filters = filters_ }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | filters = filters_ }


updateTags : String -> BoardConfig -> BoardConfig
updateTags tags config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig boardConfig ->
            let
                columnsConfig : Result (List Parser.DeadEnd) (List TagBoardConfig.LocalColumnConfig)
                columnsConfig =
                    Parser.run TagBoardConfig.columnConfigsParser tags
            in
            case columnsConfig of
                Ok parsedConfig ->
                    TagBoardConfig { boardConfig | columns = parsedConfig }

                _ ->
                    config


updateTitle : String -> BoardConfig -> BoardConfig
updateTitle title_ config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | title = title_ }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | title = title_ }
