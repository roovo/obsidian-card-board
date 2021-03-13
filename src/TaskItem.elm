module TaskItem exposing
    ( Completion(..)
    , Dated(..)
    , TaskItem
    , due
    , isCompleted
    , isDated
    , parser
    , title
    )

import Date exposing (Date)
import Maybe.Extra as ME
import Parser exposing (..)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd, nonEmptyStringParser)



-- TYPES


type TaskItem
    = TaskItem Completion Dated String


type Completion
    = Incomplete
    | Completed


type Dated
    = Undated
    | Due Date



-- INFO


title : TaskItem -> String
title (TaskItem _ _ t) =
    t


isCompleted : TaskItem -> Bool
isCompleted (TaskItem c _ _) =
    case c of
        Incomplete ->
            False

        Completed ->
            True


due : TaskItem -> Maybe Date
due (TaskItem _ d _) =
    case d of
        Undated ->
            Nothing

        Due date ->
            Just date


isDated : TaskItem -> Bool
isDated taskItem =
    taskItem
        |> due
        |> ME.isJust



-- SERIALIZATION


parser : Maybe String -> Parser TaskItem
parser fileDate =
    succeed TaskItem
        |= prefixParser
        |. chompWhile isSpaceOrTab
        |= fileDateParser fileDate
        |= nonEmptyStringParser
        |. lineEndOrEnd


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


fileDateParser : Maybe String -> Parser Dated
fileDateParser fileDate =
    fileDate
        |> Maybe.map Date.fromIsoString
        |> Maybe.map Result.toMaybe
        |> ME.join
        |> Maybe.map Due
        |> Maybe.withDefault Undated
        |> succeed
