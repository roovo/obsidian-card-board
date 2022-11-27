module TextDirection exposing
    ( TextDirection(..)
    , default
    , fromRtlFlag
    , isRtl
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



-- INFO


isRtl : TextDirection -> Bool
isRtl td =
    case td of
        LeftToRight ->
            False

        RightToLeft ->
            True
