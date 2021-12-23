module Helpers.DecodeHelpers exposing
    ( DecodeResult
    , runDecoder
    )

import TsJson.Decode as TsDecode


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
