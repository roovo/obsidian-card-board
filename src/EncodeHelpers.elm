module EncodeHelpers exposing (dateEncoder)

import Date exposing (Date)
import TsJson.Encode as TsEncode


dateEncoder : TsEncode.Encoder Date
dateEncoder =
    TsEncode.map Date.toRataDie TsEncode.int
