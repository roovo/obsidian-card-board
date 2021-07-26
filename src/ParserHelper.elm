module ParserHelper exposing
    ( anyLineParser
    , checkWhitespaceFollows
    , dateParser
    , isLineEnd
    , isSpaceOrTab
    , lineEndOrEnd
    , nonEmptyStringParser
    , wordParser
    )

import Date exposing (Date)
import Parser exposing (..)



-- TYPES


type ParseResult b
    = ParseResult b Bool



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


isNotWhitespace : Char -> Bool
isNotWhitespace char =
    not <| isSpaceTabOrLineEnd char



-- PARSERS


anyLineParser : Parser ()
anyLineParser =
    succeed ()
        |. nonEmptyLineParser
        |. lineEnd


nonEmptyLineParser : Parser String
nonEmptyLineParser =
    getChompedString chompWithEndOfLine
        |> andThen (checkIfEmpty "nonEmptyLineParser")


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
        |> andThen (checkIfEmpty "nonEmptyStringParser")


wordParser : Parser String
wordParser =
    getChompedString chompToEndOfWord
        |> andThen (checkIfEmpty "wordParser")
        |> checkWhitespaceFollows


checkWhitespaceFollows : Parser a -> Parser a
checkWhitespaceFollows xp =
    succeed ParseResult
        |= backtrackable xp
        |= oneOf
            [ map (\_ -> True) (backtrackable (chompIf isNotWhitespace))
            , succeed False
            ]
        |> andThen checkEnding


checkEnding : ParseResult b -> Parser b
checkEnding (ParseResult p isBadEnding) =
    if isBadEnding then
        problem "expecting whitespace after the parsed token"

    else
        commit p



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


checkIfEmpty : String -> String -> Parser String
checkIfEmpty calledFrom parsedString =
    if String.length parsedString == 0 then
        problem <| "Empty string found in " ++ calledFrom

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
