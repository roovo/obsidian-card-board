module ParserHelper exposing
    ( anyLineParser
    , dateParser
    , isLineEnd
    , isSpaceOrTab
    , lineEndOrEnd
    , nonEmptyStringParser
    , wordParser
    )

import Date exposing (Date)
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


isSpaceTabOrLineEnd : Char -> Bool
isSpaceTabOrLineEnd char =
    isSpaceOrTab char || isLineEnd char



-- PARSERS


anyLineParser : Parser String
anyLineParser =
    getChompedString (chompUntilEndOr "\n")
        |> andThen (consumeSeparator '\n')


dateParser : Parser Date
dateParser =
    let
        convertToDate dateString =
            dateString
                |> Date.fromIsoString
                |> Result.map succeed
                |> Result.withDefault (problem "not a valid date")
    in
    getChompedString (chompWhile <| \c -> Char.isDigit c || c == '-')
        |> andThen convertToDate


lineEndOrEnd : Parser ()
lineEndOrEnd =
    oneOf
        [ lineEnd
        , end
        ]


nonEmptyStringParser : Parser String
nonEmptyStringParser =
    getChompedString chompToEndOfLine
        |> andThen checkIfEmpty


wordParser : Parser String
wordParser =
    getChompedString chompToEndOfWord
        |> andThen (consumeSeparator ' ')



-- HELPERS


consumeSeparator : Char -> String -> Parser String
consumeSeparator separator parsedString =
    let
        checkIfAtEndOfInput : Int -> Int -> Parser String
        checkIfAtEndOfInput preSeparatorOffset postSeparatorOffset =
            if preSeparatorOffset == postSeparatorOffset && String.length parsedString == 0 then
                problem "Reached end of input"

            else
                succeed parsedString
    in
    (Parser.succeed checkIfAtEndOfInput
        |= Parser.getOffset
        |. Parser.oneOf
            [ Parser.end
            , chompIf (\c -> c == separator)
            , succeed ()
            ]
        |= Parser.getOffset
    )
        |> Parser.andThen identity


chompToEndOfLine : Parser ()
chompToEndOfLine =
    chompWhile (not << isLineEnd)


chompToEndOfWord : Parser ()
chompToEndOfWord =
    succeed ()
        |. chompWhile (not << isSpaceTabOrLineEnd)


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
