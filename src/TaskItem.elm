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


prefixParser : Parser Completion
prefixParser =
    oneOf
        [ succeed Incomplete
            |. symbol "- [ ] "
        , succeed Completed
            |. symbol "- [x] "
        , succeed Completed
            |. symbol "- [X] "
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
    case char of
        '\n' ->
            True

        -- '\u{000D}' ->
        --     True
        _ ->
            False


checkIfEmpty : String -> Parser String
checkIfEmpty parsedString =
    if String.length parsedString == 0 then
        problem "expecting a title for the Task Item"

    else
        succeed parsedString
