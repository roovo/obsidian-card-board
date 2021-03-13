module TaskItem exposing
    ( Completion(..)
    , TaskItem
    , isCompleted
    , parser
    , title
    )

import Parser exposing (..)



-- TYPES


type TaskItem
    = TaskItem Completion String


type Completion
    = Incomplete
    | Completed



-- INFO


title : TaskItem -> String
title (TaskItem _ t) =
    t


isCompleted : TaskItem -> Bool
isCompleted (TaskItem c _) =
    case c of
        Incomplete ->
            False

        Completed ->
            True



-- SERIALIZATION


parser : Parser TaskItem
parser =
    succeed TaskItem
        |= prefixParser
        |. chompWhile isSpaceOrTab
        |= nonEmptyStringParser
        |. lineEndOrEnd


lineEndOrEnd : Parser ()
lineEndOrEnd =
    oneOf
        [ lineEnd
        , end
        ]


lineEnd : Parser ()
lineEnd =
    chompWhile (\c -> c == '\n' || c == carriageReturn)


carriageReturn : Char
carriageReturn =
    Char.fromCode 13


prefixParser : Parser Completion
prefixParser =
    oneOf
        [ succeed Incomplete
            |. token "- [ ] "
        , succeed Completed
            |. token "- [x] "
        , succeed Completed
            |. token "- [X] "
        ]


isSpaceOrTab : Char -> Bool
isSpaceOrTab char =
    case char of
        ' ' ->
            True

        '\t' ->
            True

        _ ->
            False


nonEmptyStringParser : Parser String
nonEmptyStringParser =
    getChompedString chompToEndOfLine
        |> andThen checkIfEmpty


chompToEndOfLine : Parser ()
chompToEndOfLine =
    chompWhile (not << isLineEnd)


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


checkIfEmpty : String -> Parser String
checkIfEmpty parsedString =
    if String.length parsedString == 0 then
        problem "expecting a title for the Task Item"

    else
        succeed parsedString
