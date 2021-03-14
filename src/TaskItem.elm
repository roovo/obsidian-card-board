module TaskItem exposing
    ( Completion(..)
    , Dated(..)
    , TaskItem
    , due
    , filePath
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
    = TaskItem String Completion Dated String


type Completion
    = Incomplete
    | Completed


type Dated
    = Undated
    | Due Date



-- INFO


due : TaskItem -> Maybe Date
due (TaskItem _ _ d _) =
    case d of
        Undated ->
            Nothing

        Due date ->
            Just date


filePath : TaskItem -> String
filePath (TaskItem p _ _ _) =
    p


isDated : TaskItem -> Bool
isDated taskItem =
    taskItem
        |> due
        |> ME.isJust


isCompleted : TaskItem -> Bool
isCompleted (TaskItem _ c _ _) =
    case c of
        Incomplete ->
            False

        Completed ->
            True


title : TaskItem -> String
title (TaskItem _ _ _ t) =
    t



-- SERIALIZATION


parser : String -> Maybe String -> Parser TaskItem
parser pathToFile fileDate =
    succeed TaskItem
        |= succeed pathToFile
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
