module ObsidianTasksDate exposing
    ( completionTimeParser
    , dueDateParser
    )

import Date exposing (Date)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import Time



-- PARSE


dueDateParser : (Date -> a) -> Parser a
dueDateParser tagger =
    formatParser "📅 " tagger


completionTimeParser : (Time.Posix -> a) -> Parser a
completionTimeParser tagger =
    formatParser "✅ " dateToPosixTime
        |> P.map tagger



-- PRIVATE


formatParser : String -> (Date -> a) -> Parser a
formatParser emoticon tagger =
    P.succeed tagger
        |. P.token emoticon
        |= ParserHelper.dateParser


dateToPosixTime : Date.Date -> Time.Posix
dateToPosixTime date =
    Time.millisToPosix ((Date.toRataDie date - epochStartOffset) * (1000 * 60 * 60 * 24) - (1000 * 60 * 60 * 24))


epochStartOffset : Int
epochStartOffset =
    719162
