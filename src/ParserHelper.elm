module ParserHelper exposing
    ( anyLineParser
    , booleanParser
    , checkWhitespaceFollows
    , dateParser
    , indentParser
    , isLineEnd
    , isSpaceOrTab
    , lineEndOrEnd
    , nonEmptyStringParser
    , spaces
    , wordParser
    )

import Date exposing (Date)
import Parser as P exposing ((|.), (|=), Parser)



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


booleanParser : Parser Bool
booleanParser =
    P.oneOf
        [ P.succeed True
            |. P.keyword "true"
        , P.succeed False
            |. P.keyword "false"
        ]


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


step : Parser a -> List a -> Parser (P.Step (List a) (List a))
step parser_ values =
    let
        finish =
            P.Done (List.reverse values)

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


type alias NextParser a =
    { smaller : P.Parser a
    , exactly : P.Parser a
    , larger : P.Parser a
    , ending : P.Parser a
    }


indented : NextParser a -> P.Parser a
indented next =
    let
        proceed : ( Int, Int ) -> P.Parser a
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


spaces : Parser ()
spaces =
    P.chompWhile isSpaceOrTab


spacesOrLineEnd : Parser ()
spacesOrLineEnd =
    P.chompWhile isSpaceTabOrLineEnd


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


nonEmptyLineParser : Parser String
nonEmptyLineParser =
    P.getChompedString chompWithEndOfLine
        |> P.andThen (checkIfEmpty "nonEmptyLineParser")


dateParser : Parser Date
dateParser =
    let
        convertToDate dateString =
            dateString
                |> Date.fromIsoString
                |> Result.map P.succeed
                |> Result.withDefault (P.problem "not a valid date")
    in
    P.getChompedString (P.chompWhile <| \c -> Char.isDigit c || c == '-')
        |> P.andThen convertToDate


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


wordParser : Parser String
wordParser =
    P.getChompedString chompToEndOfWord
        |> P.andThen (checkIfEmpty "wordParser")
        |> checkWhitespaceFollows


checkWhitespaceFollows : Parser a -> Parser a
checkWhitespaceFollows xp =
    P.succeed ParseResult
        |= P.backtrackable xp
        |= P.oneOf
            [ P.map (\_ -> True) (P.backtrackable (P.chompIf isNotWhitespace))
            , P.succeed False
            ]
        |> P.andThen checkEnding


checkEnding : ParseResult b -> Parser b
checkEnding (ParseResult p isBadEnding) =
    if isBadEnding then
        P.problem "expecting whitespace after the parsed token"

    else
        P.commit p



-- HELPERS


consumeSeparator : Char -> String -> Parser String
consumeSeparator separator parsedString =
    let
        checkIfAtEndOfInput : Int -> Int -> Parser String
        checkIfAtEndOfInput preSeparatorOffset postSeparatorOffset =
            if preSeparatorOffset == postSeparatorOffset && String.length parsedString == 0 then
                P.problem "Reached end of input"

            else
                P.succeed parsedString
    in
    (P.succeed checkIfAtEndOfInput
        |= P.getOffset
        |. P.oneOf
            [ P.end
            , P.chompIf (\c -> c == separator)
            , P.succeed ()
            ]
        |= P.getOffset
    )
        |> P.andThen identity


chompToEndOfLine : Parser ()
chompToEndOfLine =
    P.chompWhile (not << isLineEnd)


chompToEndOfWord : Parser ()
chompToEndOfWord =
    P.succeed ()
        |. P.chompWhile (not << isSpaceTabOrLineEnd)


checkIfEmpty : String -> String -> Parser String
checkIfEmpty calledFrom parsedString =
    if String.length parsedString == 0 then
        P.problem <| "Empty string found in " ++ calledFrom

    else
        P.succeed parsedString


chompWithEndOfLine : Parser ()
chompWithEndOfLine =
    P.succeed ()
        |. P.chompWhile (not << isLineEnd)
        |. P.chompIf isLineEnd


lineEnd : Parser ()
lineEnd =
    P.chompWhile (\c -> c == '\n' || c == carriageReturn)


carriageReturn : Char
carriageReturn =
    Char.fromCode 13
