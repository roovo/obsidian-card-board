module TextDirection exposing
    ( TextDirection(..)
    , default
    , fromRtlFlag
    )

import TsJson.Decode as TsDecode



-- TYPES


type TextDirection
    = LeftToRight
    | RightToLeft


default : TextDirection
default =
    LeftToRight


fromRtlFlag : Bool -> TextDirection
fromRtlFlag b =
    if b then
        RightToLeft

    else
        LeftToRight
