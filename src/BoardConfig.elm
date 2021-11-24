module BoardConfig exposing
    ( BoardConfig(..)
    , decoder
    , decoder_v_0_1_0
    , default
    , encoder
    , fromBoardType
    , isForDateBoard
    , isForTagBoard
    , title
    , toggleIncludeOthers
    , toggleIncludeUndated
    , toggleIncludeUntagged
    , updateBoardType
    , updateCompletedCount
    , updateTags
    , updateTitle
    )

import DateBoard
import DecodeHelpers
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


title : BoardConfig -> String
title config =
    case config of
        DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title



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


toggleIncludeUntagged : BoardConfig -> BoardConfig
toggleIncludeUntagged config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig tagBoardConfig ->
            TagBoardConfig { tagBoardConfig | includeUntagged = not tagBoardConfig.includeUntagged }


toggleIncludeUndated : BoardConfig -> BoardConfig
toggleIncludeUndated config =
    case config of
        DateBoardConfig dateBoardConfig ->
            DateBoardConfig { dateBoardConfig | includeUndated = not dateBoardConfig.includeUndated }

        TagBoardConfig _ ->
            config


toggleIncludeOthers : BoardConfig -> BoardConfig
toggleIncludeOthers config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig tagBoardConfig ->
            TagBoardConfig { tagBoardConfig | includeOthers = not tagBoardConfig.includeOthers }


updateTitle : String -> BoardConfig -> BoardConfig
updateTitle title_ config =
    case config of
        DateBoardConfig dateBoardConfig ->
            DateBoardConfig { dateBoardConfig | title = title_ }

        TagBoardConfig tagBoardConfig ->
            TagBoardConfig { tagBoardConfig | title = title_ }


updateTags : String -> BoardConfig -> BoardConfig
updateTags tags config =
    case config of
        DateBoardConfig _ ->
            config

        TagBoardConfig tagBoardConfig ->
            let
                columnsConfig =
                    Parser.run TagBoard.columnConfigsParser tags
            in
            case columnsConfig of
                Ok parsedConfig ->
                    TagBoardConfig { tagBoardConfig | columns = parsedConfig }

                _ ->
                    config


updateCompletedCount : Maybe Int -> BoardConfig -> BoardConfig
updateCompletedCount value config =
    case ( config, value ) of
        ( DateBoardConfig dateBoardConfig, Just newCount ) ->
            DateBoardConfig { dateBoardConfig | completedCount = newCount }

        ( TagBoardConfig tagBoardConfig, Just newCount ) ->
            TagBoardConfig { tagBoardConfig | completedCount = newCount }

        _ ->
            config


updateBoardType : String -> BoardConfig -> BoardConfig
updateBoardType boardType config =
    fromBoardType boardType (title config)
