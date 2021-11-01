module BoardConfig exposing
    ( BoardConfig(..)
    , decoder
    , defaultConfig
    , encoder
    , isForDateBoard
    , isForTagBoard
    , title
    )

import DateBoard
import DecodeHelpers
import TagBoard
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type BoardConfig
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config


defaultConfig : BoardConfig
defaultConfig =
    TagBoardConfig TagBoard.defaultConfig



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
