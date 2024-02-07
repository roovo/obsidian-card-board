module DecodeHelpers exposing
    ( toElmVariant
    , toElmVariant0
    )

import Json.Encode as JE
import TsJson.Decode as TsDecode


toElmVariant : String -> (value -> a) -> TsDecode.Decoder value -> TsDecode.Decoder a
toElmVariant tagName constructor decoder_ =
    TsDecode.field "tag" (TsDecode.literal constructor (JE.string tagName))
        |> TsDecode.andMap (TsDecode.field "data" decoder_)


toElmVariant0 : String -> a -> TsDecode.Decoder a
toElmVariant0 tagName constructor =
    TsDecode.field "tag" (TsDecode.literal constructor (JE.string tagName))
