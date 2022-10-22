module ParserHelper exposing
    ( anyLineParser
    , booleanParser
    , checkIfEmpty
    , checkIsNotNumeric
    , checkWhitespaceFollows
    , dateParser
    , indentParser
    , isSpaceOrTab
    , lineEndOrEnd
    , nonEmptyStringParser
    , spaces
    , timeParser
    , wordParser
    )

import Date exposing (Date)
import Iso8601
import Parser as P exposing ((|.), (|=), Parser)
import Time



-- TYPES


type ParseResult b
    = ParseResult b Bool



-- TESTS


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


anyLineParser : Parser String
anyLineParser =
    let
        removeTrailingEol : String -> Parser String
        removeTrailingEol parsedLine =
            P.succeed <| String.dropRight 1 parsedLine
    in
    P.succeed identity
        |= nonEmptyLineParser
        |. lineEnd
        |> P.andThen removeTrailingEol


booleanParser : Parser Bool
booleanParser =
    P.oneOf
        [ P.succeed True
            |. P.keyword "true"
        , P.succeed False
            |. P.keyword "false"
        ]


checkIsNotNumeric : String -> String -> Parser String
checkIsNotNumeric calledFrom parsedString =
    case String.toInt parsedString of
        Just _ ->
            P.problem <| "Numeric string found in " ++ calledFrom

        Nothing ->
            P.succeed parsedString


checkWhitespaceFollows : Parser a -> Parser a
checkWhitespaceFollows xp =
    P.succeed ParseResult
        |= P.backtrackable xp
        |= P.oneOf
            [ P.map (\_ -> True) (P.backtrackable (P.chompIf isNotWhitespace))
            , P.succeed False
            ]
        |> P.andThen checkEnding


dateParser : Parser Date
dateParser =
    let
        convertToDate : String -> Parser Date
        convertToDate dateString =
            dateString
                |> Date.fromIsoString
                |> Result.map P.succeed
                |> Result.withDefault (P.problem "not a valid date")
    in
    P.getChompedString (P.chompWhile <| \c -> Char.isDigit c || c == '-')
        |> P.andThen convertToDate


indentParser : Parser a -> Parser (List a)
indentParser parser =
    let
        list_ : ( Int, Int ) -> Parser (List a)
        list_ ( indent, column ) =
            if column > indent then
                P.withIndent (indent + 1) parser_

            else
                P.succeed []

        parser_ : Parser (List a)
        parser_ =
            P.succeed identity
                |= P.loop [] (step parser)
    in
    P.oneOf
        [ P.succeed (\i c -> ( i, c ))
            |. spaces
            |= P.getIndent
            |= P.getCol
            |> P.andThen list_
        , P.succeed []
        ]


lineEndOrEnd : Parser ()
lineEndOrEnd =
    P.oneOf
        [ lineEnd
        , P.end
        ]


nonEmptyStringParser : Parser String
nonEmptyStringParser =
    P.getChompedString chompToEndOfLine
        |> P.andThen (checkIfEmpty "nonEmptyStringParser")


spaces : Parser ()
spaces =
    P.chompWhile isSpaceOrTab


timeParser : Parser Time.Posix
timeParser =
    let
        convertToTime : String -> Parser Time.Posix
        convertToTime timeString =
            timeString
                |> Iso8601.toTime
                |> Result.map P.succeed
                |> Result.withDefault (P.problem "not a valid date")
    in
    P.getChompedString (P.chompWhile <| \c -> Char.isDigit c || c == '-' || c == 'T' || c == ':')
        |> P.andThen convertToTime


wordParser : Parser String
wordParser =
    P.getChompedString chompToEndOfWord
        |> P.andThen (checkIfEmpty "wordParser")
        |> checkWhitespaceFollows



-- PRIVATE


carriageReturn : Char
carriageReturn =
    Char.fromCode 13


checkEnding : ParseResult b -> Parser b
checkEnding (ParseResult p isBadEnding) =
    if isBadEnding then
        P.problem "expecting whitespace after the parsed token"

    else
        P.commit p


checkIfEmpty : String -> String -> Parser String
checkIfEmpty calledFrom parsedString =
    if String.length parsedString == 0 then
        P.problem <| "Empty string found in " ++ calledFrom

    else
        P.succeed parsedString


chompToEndOfLine : Parser ()
chompToEndOfLine =
    P.chompWhile (not << isLineEnd)


chompToEndOfWord : Parser ()
chompToEndOfWord =
    P.succeed ()
        |. P.chompWhile (not << isSpaceTabOrLineEnd)


chompWithEndOfLine : Parser ()
chompWithEndOfLine =
    P.succeed ()
        |. P.chompWhile (not << isLineEnd)
        |. P.chompIf isLineEnd


type alias NextParser a =
    { smaller : Parser a
    , exactly : Parser a
    , larger : Parser a
    , ending : Parser a
    }


indented : NextParser a -> Parser a
indented next =
    let
        proceed : ( Int, Int ) -> Parser a
        proceed ( minimal, actual ) =
            P.oneOf
                [ P.andThen (\_ -> next.ending) P.end
                , if actual == minimal then
                    next.exactly

                  else if actual > minimal then
                    next.larger

                  else
                    next.smaller
                ]
    in
    P.succeed (\a b -> ( a, b ))
        |= P.getIndent
        |. spacesOrLineEnd
        |= P.getCol
        |> P.andThen proceed


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


isNotWhitespace : Char -> Bool
isNotWhitespace char =
    not <| isSpaceTabOrLineEnd char


isSpaceTabOrLineEnd : Char -> Bool
isSpaceTabOrLineEnd char =
    isSpaceOrTab char || isLineEnd char


lineEnd : Parser ()
lineEnd =
    P.chompWhile (\c -> c == '\n' || c == carriageReturn)


nonEmptyLineParser : Parser String
nonEmptyLineParser =
    P.getChompedString chompWithEndOfLine
        |> P.andThen (checkIfEmpty "nonEmptyLineParser")


spacesOrLineEnd : Parser ()
spacesOrLineEnd =
    P.chompWhile isSpaceTabOrLineEnd


step : Parser a -> List a -> Parser (P.Step (List a) (List a))
step parser_ values =
    let
        finish : P.Step (List a) (List a)
        finish =
            P.Done (List.reverse values)

        next : a -> P.Step (List a) (List a)
        next value_ =
            P.Loop (value_ :: values)
    in
    indented
        { smaller =
            P.succeed finish
        , exactly =
            P.oneOf
                [ P.succeed next
                    |= parser_
                , P.succeed finish
                ]
        , larger =
            P.oneOf
                [ P.succeed next
                    |= parser_
                , P.succeed finish
                ]
        , ending =
            P.succeed finish
        }
