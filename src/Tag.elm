module Tag exposing
    ( parser
    , toString
    )

import Parser as P exposing ((|.), (|=), Parser)


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
                |> P.andThen checkIfEmpty
                |> P.andThen checkIsNotNumeric
           )



-- PRIVATE


checkIsNotNumeric : String -> Parser String
checkIsNotNumeric parsedString =
    case String.toInt parsedString of
        Just _ ->
            P.problem "numeric tag"

        Nothing ->
            P.succeed parsedString


checkIfEmpty : String -> Parser String
checkIfEmpty parsedString =
    if String.length parsedString == 0 then
        P.problem "Empty string found"

    else
        P.succeed parsedString


isInvalidCharacter : Char -> Bool
isInvalidCharacter c =
    if Char.isAlphaNum c then
        False

    else if List.member c [ '_', '-', '/' ] then
        False

    else
        True
