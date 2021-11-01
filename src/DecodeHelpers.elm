module DecodeHelpers exposing (toElmVariant)

import Json.Encode as JE
import TsJson.Decode as TsDecode


toElmVariant : String -> (value -> a) -> TsDecode.Decoder value -> TsDecode.Decoder a
toElmVariant tagName constructor decoder_ =
    TsDecode.field "tag" (TsDecode.literal constructor (JE.string tagName))
        |> TsDecode.andMap (TsDecode.field "data" decoder_)
