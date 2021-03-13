module TaskItem exposing
    ( Completion(..)
    , TaskItem
    , isCompleted
    , parser
    , title
    )

import Parser exposing (..)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd, nonEmptyStringParser)



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
