module TextDirection exposing
    ( TextDirection(..)
    , default
    , fromRtlFlag
    , toString
    )

import TsJson.Decode as TsDecode



-- TYPES


type TextDirection
    = LeftToRight
    | RightToLeft



-- CREATE


default : TextDirection
default =
    LeftToRight


fromRtlFlag : Bool -> TextDirection
fromRtlFlag b =
    if b then
        RightToLeft

    else
        LeftToRight



-- CONVERSION


toString : TextDirection -> String
toString td =
    case td of
        LeftToRight ->
            "ltr"

        RightToLeft ->
            "rtl"
