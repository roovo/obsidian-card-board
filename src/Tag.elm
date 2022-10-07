module Tag exposing
    ( Tag
    , parser
    , startsWith
    , toString
    )

import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper


type Tag
    = Tag String



-- CREATE


parser : Parser Tag
parser =
    P.succeed Tag
        |. P.token "#"
        |= (P.getChompedString (P.chompWhile (not << isInvalidCharacter))
                |> P.andThen (ParserHelper.checkIfEmpty "Tag.parser")
                |> P.andThen (ParserHelper.checkIsNotNumeric "Tag.parser")
                |> ParserHelper.checkWhitespaceFollows
           )



-- UTILITIES


startsWith : String -> Tag -> Bool
startsWith substring (Tag tagString) =
    String.startsWith substring tagString



-- CONVERT


toString : Tag -> String
toString (Tag s) =
    s



-- PRIVATE


isInvalidCharacter : Char -> Bool
isInvalidCharacter c =
    if Char.isAlphaNum c then
        False

    else if List.member c [ '_', '-', '/' ] then
        False

    else
        True
