module DataviewDate exposing
    ( completionTimeParser
    , dueDateParser
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import Date exposing (Date)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper exposing (checkWhitespaceFollows)
import Time


completionTimeParser : DataviewTaskCompletion -> (Time.Posix -> a) -> Parser a
completionTimeParser dataviewTaskCompletion tagger =
    case dataviewTaskCompletion of
        DataviewTaskCompletion.Text tag ->
            parser tag ParserHelper.dateToPosixTime
                |> P.map tagger

        _ ->
            parser "completion" ParserHelper.dateToPosixTime
                |> P.map tagger


dueDateParser : (Date -> a) -> Parser a
dueDateParser =
    parser "due"



-- PRIVATE


parser : String -> (Date -> b) -> Parser b
parser tagKeyword tagger =
    tagParser tagKeyword tagger
        |> checkWhitespaceFollows


tagParser : String -> (Date -> b) -> Parser b
tagParser tagKeyword tagger =
    P.succeed tagger
        |. P.token ("[" ++ tagKeyword ++ ":: ")
        |= ParserHelper.dateParser
        |. P.token "]"
