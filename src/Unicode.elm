module Unicode exposing
    ( basicLatinEndCode
    , forwardslashCode
    , isWhitespace
    , minusCode
    , underscoreCode
    )

-- CODES


basicLatinEndCode : Int
basicLatinEndCode =
    0x7F


forwardslashCode : Int
forwardslashCode =
    0x2F


minusCode : Int
minusCode =
    0x2D


underscoreCode : Int
underscoreCode =
    0x5F



-- CHECKS


isWhitespace : Char -> Bool
isWhitespace c =
    let
        code : Int
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


generalPunctuationStartCode : Int
generalPunctuationStartCode =
    0x2000


hairSpaceCode : Int
hairSpaceCode =
    0x200A


ideographicSpaceCode : Int
ideographicSpaceCode =
    0x3000


lineSepratorCode : Int
lineSepratorCode =
    0x2028


mediumMathsSpaceCode : Int
mediumMathsSpaceCode =
    0x205F


nbspCode : Int
nbspCode =
    0xA0


spaceCode : Int
spaceCode =
    0x20
