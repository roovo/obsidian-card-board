module Filter exposing
    ( Filter(..)
    , decoder
    , encoder
    )

import DecodeHelpers
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


type Filter
    = PathFilter String
    | TagFilter String



-- SERIALISATION


encoder : TsEncode.Encoder Filter
encoder =
    TsEncode.union
        (\vPathFilter vTagFilter value ->
            case value of
                PathFilter path ->
                    vPathFilter path

                TagFilter tag ->
                    vTagFilter tag
        )
        |> TsEncode.variantTagged "pathFilter" TsEncode.string
        |> TsEncode.variantTagged "tagFilter" TsEncode.string
        |> TsEncode.buildUnion


decoder : TsDecode.Decoder Filter
decoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "pathFilter" PathFilter TsDecode.string
        , DecodeHelpers.toElmVariant "tagFilter" TagFilter TsDecode.string
        ]
