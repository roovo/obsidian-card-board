module Tag exposing
    ( Tag
    , equals
    , parser
    , startsWith
    , toString
    )

import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import Unicode



-- TYPES


type Tag
    = Tag String



-- CREATE


parser : Parser Tag
parser =
    P.succeed Tag
        |. P.token "#"
        |= (P.getChompedString (P.chompWhile isValidTagCharacter)
                |> P.andThen (ParserHelper.checkIfEmpty "Tag.parser")
                |> P.andThen (ParserHelper.checkIsNotNumeric "Tag.parser")
           )



-- UTILITIES


equals : String -> Tag -> Bool
equals checkString (Tag tagString) =
    checkString == tagString


startsWith : String -> Tag -> Bool
startsWith substring (Tag tagString) =
    String.startsWith substring tagString



-- CONVERT


toString : Tag -> String
toString (Tag s) =
    s



-- PRIVATE


isValidTagCharacter : Char -> Bool
isValidTagCharacter c =
    let
        code : Int
        code =
            Char.toCode c
    in
    Char.isAlphaNum c
        || (code == Unicode.minusCode)
        || (code == Unicode.forwardslashCode)
        || (code == Unicode.underscoreCode)
        || (code > Unicode.basicLatinEndCode && not (Unicode.isWhitespace c))
