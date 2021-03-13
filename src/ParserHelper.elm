module ParserHelper exposing
    ( anyLineParser
    , isLineEnd
    , isSpaceOrTab
    , lineEndOrEnd
    , nonEmptyLineParser
    , nonEmptyStringParser
    )

import Parser exposing (..)



-- TESTS


isLineEnd : Char -> Bool
isLineEnd char =
    case Char.toCode char of
        10 ->
            -- new line
            True

        13 ->
            -- carriage return
            True

        _ ->
            False


isSpaceOrTab : Char -> Bool
isSpaceOrTab char =
    case char of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False



-- PARSERS


anyLineParser : Parser ()
anyLineParser =
    succeed ()
        |. nonEmptyLineParser
        |. lineEnd


nonEmptyStringParser : Parser String
nonEmptyStringParser =
    getChompedString chompToEndOfLine
        |> andThen checkIfEmpty


nonEmptyLineParser : Parser String
nonEmptyLineParser =
    getChompedString chompWithEndOfLine
        |> andThen checkIfEmpty


lineEndOrEnd : Parser ()
lineEndOrEnd =
    oneOf
        [ lineEnd
        , end
        ]



-- HELPERS


chompToEndOfLine : Parser ()
chompToEndOfLine =
    chompWhile (not << isLineEnd)


checkIfEmpty : String -> Parser String
checkIfEmpty parsedString =
    if String.length parsedString == 0 then
        problem ""

    else
        succeed parsedString


chompWithEndOfLine : Parser ()
chompWithEndOfLine =
    succeed ()
        |. chompWhile (not << isLineEnd)
        |. chompIf isLineEnd


lineEnd : Parser ()
lineEnd =
    chompWhile (\c -> c == '\n' || c == carriageReturn)


carriageReturn : Char
carriageReturn =
    Char.fromCode 13
