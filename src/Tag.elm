module Tag exposing
    ( Tag
    , parser
    , toString
    )

import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper


type Tag
    = Tag String


toString : Tag -> String
toString (Tag s) =
    s


parser : Parser Tag
parser =
    P.succeed Tag
        |. P.token "#"
        |= (P.getChompedString (P.chompWhile (not << isInvalidCharacter))
                |> P.andThen (ParserHelper.checkIfEmpty "Tag.parser")
                |> P.andThen (ParserHelper.checkIsNotNumeric "Tag.parser")
                |> ParserHelper.checkWhitespaceFollows
           )



-- PRIVATE


isInvalidCharacter : Char -> Bool
isInvalidCharacter c =
    if Char.isAlphaNum c then
        False

    else if List.member c [ '_', '-', '/' ] then
        False

    else
        True
