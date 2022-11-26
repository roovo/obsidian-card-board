module Unicode exposing
    ( basicLatinEndCode
    , forwardslashCode
    , isWhitespace
    , minusCode
    , underscoreCode
    )

-- CODES


basicLatinEndCode =
    0x7F


forwardslashCode =
    0x2F


minusCode =
    0x2D


underscoreCode =
    0x5F



-- CHECKS


isWhitespace : Char -> Bool
isWhitespace c =
    let
        code =
            Char.toCode c
    in
    (code == spaceCode)
        || (code == nbspCode)
        || (code == lineSepratorCode)
        || (code == mediumMathsSpaceCode)
        || (code == ideographicSpaceCode)
        || (generalPunctuationStartCode <= code && code <= hairSpaceCode)



-- PRIVATE


generalPunctuationStartCode =
    0x2000


hairSpaceCode =
    0x200A


ideographicSpaceCode =
    0x3000


lineSepratorCode =
    0x2028


mediumMathsSpaceCode =
    0x205F


nbspCode =
    0xA0


spaceCode =
    0x20
