module Tag exposing
    ( Tag
    , equals
    , parser
    , startsWith
    , toString
    )

import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper



-- TYPES


type Tag
    = Tag String



-- CREATE


parser : Parser Tag
parser =
    P.succeed Tag
        |. P.token "#"
        |= (P.getChompedString (P.chompWhile isValidCharacter)
                |> P.andThen (ParserHelper.checkIfEmpty "Tag.parser")
                |> P.andThen (ParserHelper.checkIsNotNumeric "Tag.parser")
                |> ParserHelper.checkWhitespaceFollows
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


isValidCharacter : Char -> Bool
isValidCharacter c =
    let
        code =
            Char.toCode c
    in
    Char.isAlphaNum c || code == 0x2D || code == 0x2F || code == 0x5F || code >= 0xA1
