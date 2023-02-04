module ObsidianTasksDate exposing
    ( completionTimeParser
    , dueDateParser
    )

import Date exposing (Date)
import DueDate exposing (DueDate)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import Time



-- PARSE


dueDateParser : (DueDate -> a) -> Parser a
dueDateParser tagger =
    formatParser "📅 " identity
        |> P.map DueDate.SetToDate
        |> P.map tagger


completionTimeParser : (Time.Posix -> a) -> Parser a
completionTimeParser tagger =
    formatParser "✅ " ParserHelper.dateToPosixTime
        |> P.map tagger



-- PRIVATE


formatParser : String -> (Date -> a) -> Parser a
formatParser emoticon tagger =
    P.succeed tagger
        |. P.token emoticon
        |= ParserHelper.dateParser
