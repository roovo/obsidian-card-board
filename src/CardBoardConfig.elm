module CardBoardConfig exposing
    ( Config(..)
    , configsEncoder
    , decoder
    , defaultConfig
    , isDateBoard
    , isTagBoard
    , title
    )

import DateBoard
import DecodeHelpers
import TagBoard
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type Config
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config



-- INFO


isDateBoard : Config -> Bool
isDateBoard config =
    case config of
        DateBoardConfig _ ->
            True

        _ ->
            False


isTagBoard : Config -> Bool
isTagBoard config =
    case config of
        TagBoardConfig _ ->
            True

        _ ->
            False


defaultConfig : Config
defaultConfig =
    TagBoardConfig TagBoard.defaultConfig


title : Config -> String
title config =
    case config of
        DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title



-- SERIALIZATION


configsEncoder : TsEncode.Encoder { boardConfigs : List Config }
configsEncoder =
    TsEncode.object
        [ TsEncode.required "boardConfigs" .boardConfigs (TsEncode.list encoder)
        ]


decoder : TsDecode.Decoder Config
decoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "dateBoardConfig" DateBoardConfig DateBoard.configDecoder
        , DecodeHelpers.toElmVariant "tagBoardConfig" TagBoardConfig TagBoard.configDecoder
        ]



-- HELPERS


encoder : TsEncode.Encoder Config
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
