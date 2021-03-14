module TaskItem exposing
    ( Completion(..)
    , Dated(..)
    , TaskItem
    , completed
    , forFuture
    , forToday
    , forTomorrow
    , fromFile
    , parser
    , title
    , undated
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


title : TaskItem -> String
title (TaskItem _ _ _ t) =
    t



-- FILTERS


undated : List TaskItem -> List TaskItem
undated =
    List.filter (\t -> (not <| isCompleted t) && (not <| isDated t))


forToday : Date -> List TaskItem -> List TaskItem
forToday today =
    let
        isToday t =
            case due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days today date <= 0 then
                        True

                    else
                        False
    in
    List.filter (\t -> (not <| isCompleted t) && isToday t)


forTomorrow : Date -> List TaskItem -> List TaskItem
forTomorrow today =
    let
        tomorrow =
            Date.add Date.Days 1 today

        isTomorrow t =
            case due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days tomorrow date == 0 then
                        True

                    else
                        False
    in
    List.filter (\t -> isTomorrow t && (not <| isCompleted t))


forFuture : Date -> List TaskItem -> List TaskItem
forFuture today =
    let
        tomorrow =
            Date.add Date.Days 1 today

        isToday t =
            case due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days tomorrow date > 0 then
                        True

                    else
                        False
    in
    List.filter (\t -> (not <| isCompleted t) && isToday t)


completed : List TaskItem -> List TaskItem
completed =
    List.filter isCompleted


fromFile : String -> List TaskItem -> List TaskItem
fromFile pathToFile =
    List.filter <| isFromFile pathToFile



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



-- PRIVATE


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


isFromFile : String -> TaskItem -> Bool
isFromFile pathToFile (TaskItem p _ _ _) =
    p == pathToFile
