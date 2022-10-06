module BoardConfig exposing
    ( BoardConfig(..)
    , decoder_v_0_1_0
    , decoder_v_0_2_0
    , decoder_v_0_3_0
    , decoder_v_0_4_0
    , default
    , encoder
    , filterPolarity
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
    , updateBoardType
    , updateCompletedCount
    , updateFilterPolarity
    , updateFilters
    , updateTags
    , updateTitle
    )

import DateBoard
import DecodeHelpers
import Filter exposing (Filter, Polarity)
import Parser
import TagBoard
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type BoardConfig
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config


default : BoardConfig
default =
    TagBoardConfig TagBoard.defaultConfig


fromBoardType : String -> String -> BoardConfig
fromBoardType boardType title_ =
    case boardType of
        "dateBoard" ->
            let
                newBoardConfig : DateBoard.Config
                newBoardConfig =
                    DateBoard.defaultConfig
            in
            DateBoardConfig { newBoardConfig | title = title_ }

        _ ->
            let
                newBoardConfig : TagBoard.Config
                newBoardConfig =
                    TagBoard.defaultConfig
            in
            TagBoardConfig { newBoardConfig | title = title_ }



-- INFO


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


title : BoardConfig -> String
title config =
    case config of
        DateBoardConfig boardConfig ->
            boardConfig.title

        TagBoardConfig boardConfig ->
            boardConfig.title



-- SERIALIZATION


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
        |> TsEncode.variantTagged "dateBoardConfig" DateBoard.configEncoder
        |> TsEncode.variantTagged "tagBoardConfig" TagBoard.configEncoder
        |> TsEncode.buildUnion


decoder_v_0_4_0 : TsDecode.Decoder BoardConfig
decoder_v_0_4_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoard.configDecoder_v_0_4_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoard.configDecoder_v_0_4_0
        ]


decoder_v_0_3_0 : TsDecode.Decoder BoardConfig
decoder_v_0_3_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoard.configDecoder_v_0_3_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoard.configDecoder_v_0_3_0
        ]


decoder_v_0_2_0 : TsDecode.Decoder BoardConfig
decoder_v_0_2_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoard.configDecoder_v_0_2_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoard.configDecoder_v_0_2_0
        ]


decoder_v_0_1_0 : TsDecode.Decoder BoardConfig
decoder_v_0_1_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoard.configDecoder_v_0_1_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoard.configDecoder_v_0_1_0
        ]



-- MODIFICATION


mapFilters : (Filter -> Filter) -> BoardConfig -> BoardConfig
mapFilters fn config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | filters = List.map fn boardConfig.filters }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | filters = List.map fn boardConfig.filters }


toggleIncludeUntagged : BoardConfig -> BoardConfig
toggleIncludeUntagged config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | includeUntagged = not boardConfig.includeUntagged }


toggleIncludeUndated : BoardConfig -> BoardConfig
toggleIncludeUndated config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | includeUndated = not boardConfig.includeUndated }

        TagBoardConfig _ ->
            config


toggleIncludeOthers : BoardConfig -> BoardConfig
toggleIncludeOthers config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | includeOthers = not boardConfig.includeOthers }


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


updateTitle : String -> BoardConfig -> BoardConfig
updateTitle title_ config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | title = title_ }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | title = title_ }


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
                columnsConfig : Result (List Parser.DeadEnd) (List TagBoard.ColumnConfig)
                columnsConfig =
                    Parser.run TagBoard.columnConfigsParser tags
            in
            case columnsConfig of
                Ok parsedConfig ->
                    TagBoardConfig { boardConfig | columns = parsedConfig }

                _ ->
                    config


updateCompletedCount : Maybe Int -> BoardConfig -> BoardConfig
updateCompletedCount value config =
    case ( config, value ) of
        ( DateBoardConfig boardConfig, Just newCount ) ->
            DateBoardConfig { boardConfig | completedCount = newCount }

        ( TagBoardConfig boardConfig, Just newCount ) ->
            TagBoardConfig { boardConfig | completedCount = newCount }

        _ ->
            config


updateFilterPolarity : String -> BoardConfig -> BoardConfig
updateFilterPolarity polarity config =
    case config of
        DateBoardConfig boardConfig ->
            DateBoardConfig { boardConfig | filterPolarity = Filter.polarityFromString polarity }

        TagBoardConfig boardConfig ->
            TagBoardConfig { boardConfig | filterPolarity = Filter.polarityFromString polarity }


updateBoardType : String -> BoardConfig -> BoardConfig
updateBoardType boardType config =
    fromBoardType boardType (title config)
