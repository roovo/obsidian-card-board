module ParserHelper exposing
    ( anyLineParser
    , isLineEnd
    , isSpaceOrTab
    , lineEndOrEnd
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


anyLineParser : Parser String
anyLineParser =
    getChompedString (chompUntilEndOr "\n")
        |> andThen failIfEndOfInput


failIfEndOfInput : String -> Parser String
failIfEndOfInput parsedString =
    let
        succeedIfNotAtEndOfInput result =
            case result of
                Err _ ->
                    problem "Reached end of input"

                _ ->
                    succeed parsedString

        checkIfAtEndOfInput : Int -> Int -> Result String String
        checkIfAtEndOfInput pre post =
            if pre == post && String.length parsedString == 0 then
                Err ""

            else
                Ok parsedString
    in
    (Parser.succeed checkIfAtEndOfInput
        |= Parser.getOffset
        |. Parser.oneOf
            [ Parser.end
            , chompIf (\c -> c == '\n')
            ]
        |= Parser.getOffset
    )
        |> Parser.andThen succeedIfNotAtEndOfInput


nonEmptyStringParser : Parser String
nonEmptyStringParser =
    getChompedString chompToEndOfLine
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
