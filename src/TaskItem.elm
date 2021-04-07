module TaskItem exposing
    ( Completion(..)
    , Dated(..)
    , TaskItem
    , due
    , filePath
    , id
    , isCompleted
    , isDated
    , isFromFile
    , lineNumber
    , parser
    , title
    )

import Date exposing (Date)
import Maybe.Extra as ME
import Parser exposing (..)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd, nonEmptyStringParser)



-- TYPES


type TaskItem
    = TaskItem String Int Completion Dated String


type Completion
    = Incomplete
    | Completed


type Dated
    = Undated
    | Due Date



-- INFO


title : TaskItem -> String
title (TaskItem _ _ _ _ t) =
    t


due : TaskItem -> Maybe Date
due (TaskItem _ _ _ d _) =
    case d of
        Undated ->
            Nothing

        Due date ->
            Just date


filePath : TaskItem -> String
filePath (TaskItem p _ _ _ _) =
    p


id : TaskItem -> String
id (TaskItem p l _ _ _) =
    p ++ ":" ++ String.fromInt l


isDated : TaskItem -> Bool
isDated taskItem =
    taskItem
        |> due
        |> ME.isJust


isCompleted : TaskItem -> Bool
isCompleted (TaskItem _ _ c _ _) =
    case c of
        Incomplete ->
            False

        Completed ->
            True


isFromFile : String -> TaskItem -> Bool
isFromFile pathToFile (TaskItem p _ _ _ _) =
    p == pathToFile


lineNumber : TaskItem -> Int
lineNumber (TaskItem _ l _ _ _) =
    l



-- SERIALIZATION


parser : String -> Maybe String -> Parser TaskItem
parser pathToFile fileDate =
    succeed TaskItem
        |= succeed pathToFile
        |= Parser.getRow
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
