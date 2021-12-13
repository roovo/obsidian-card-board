module BoardConfig exposing
    ( BoardConfig(..)
    , decoder
    , decoder_v_0_1_0
    , default
    , encoder
    , filters
    , fromBoardType
    , isForDateBoard
    , isForTagBoard
    , title
    , toggleIncludeOthers
    , toggleIncludeUndated
    , toggleIncludeUntagged
    , updateBoardType
    , updateCompletedCount
    , updateFilters
    , updateTags
    , updateTitle
    )

import DateBoard
import DecodeHelpers
import Filter exposing (Filter)
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
                newBoardConfig =
                    DateBoard.defaultConfig
            in
            DateBoardConfig { newBoardConfig | title = title_ }

        _ ->
            let
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


decoder : TsDecode.Decoder BoardConfig
decoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoard.configDecoder
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoard.configDecoder
        ]


decoder_v_0_1_0 : TsDecode.Decoder BoardConfig
decoder_v_0_1_0 =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoard.configDecoder_v_0_1_0
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoard.configDecoder_v_0_1_0
        ]



-- MODIFICATION


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


updateBoardType : String -> BoardConfig -> BoardConfig
updateBoardType boardType config =
    fromBoardType boardType (title config)
