module DueDate exposing
    ( DueDate(..)
    , decoder
    , encoder
    )

import Date exposing (Date)
import DecodeHelpers
import EncodeHelpers
import Json.Encode as JE
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


type DueDate
    = NotSet
    | SetToDate Date
    | SetToNone



-- SERIALIZE


decoder : TsDecode.Decoder DueDate
decoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant0 "NotSet" NotSet
        , toElmSetToDate "SetToDate" SetToDate DecodeHelpers.dateDecoder
        , DecodeHelpers.toElmVariant0 "SetToNone" SetToNone
        ]


encoder : TsEncode.Encoder DueDate
encoder =
    TsEncode.union
        (\vNotSet vSetToDate vSetToNone value ->
            case value of
                NotSet ->
                    vNotSet

                SetToDate date ->
                    vSetToDate date

                SetToNone ->
                    vSetToNone
        )
        |> TsEncode.variant0 "NotSet"
        |> TsEncode.variantObject "SetToDate" [ TsEncode.required "date" identity EncodeHelpers.dateEncoder ]
        |> TsEncode.variant0 "SetToNone"
        |> TsEncode.buildUnion



-- PRIVATE


toElmSetToDate : String -> (value -> a) -> TsDecode.Decoder value -> TsDecode.Decoder a
toElmSetToDate tagName constructor decoder_ =
    TsDecode.field "tag" (TsDecode.literal constructor (JE.string tagName))
        |> TsDecode.andMap (TsDecode.field "date" decoder_)
