module TaskPaperTag exposing (doneTagParser, dueTagParser, parser)

import Date exposing (Date)
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, chompIf, commit, getOffset, map, oneOf, problem, succeed, token)
import ParserHelper exposing (checkWhitespaceFollows)



-- PARSER


parser : String -> Parser a -> (a -> b) -> Parser b
parser tagKeyword valueParser tagger =
    tagParser tagKeyword valueParser tagger
        |> checkWhitespaceFollows


doneTagParser : (Date -> a) -> Parser a
doneTagParser =
    parser "done" ParserHelper.dateParser


dueTagParser : (Date -> a) -> Parser a
dueTagParser =
    parser "due" ParserHelper.dateParser



-- PRIVATE


tagParser : String -> Parser a -> (a -> b) -> Parser b
tagParser tagKeyword valueParser tagger =
    succeed tagger
        |. token ("@" ++ tagKeyword ++ "(")
        |= valueParser
        |. token ")"
