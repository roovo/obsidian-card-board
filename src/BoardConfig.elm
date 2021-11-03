module BoardConfig exposing
    ( BoardConfig(..)
    , decoder
    , default
    , encoder
    , fromBoardType
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
