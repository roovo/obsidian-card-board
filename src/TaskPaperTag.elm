module TaskPaperTag exposing (autocompleteTagParser, completedTagParser, dueTagParser, parser)

import Date exposing (Date)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper exposing (checkWhitespaceFollows)
import Time



-- PARSER


autocompleteTagParser : (Bool -> a) -> Parser a
autocompleteTagParser =
    parser "autocomplete" ParserHelper.booleanParser


completedTagParser : (Time.Posix -> a) -> Parser a
completedTagParser =
    parser "completed" ParserHelper.timeParser


dueTagParser : (Date -> a) -> Parser a
dueTagParser =
    parser "due" ParserHelper.dateParser


parser : String -> Parser a -> (a -> b) -> Parser b
parser tagKeyword valueParser tagger =
    tagParser tagKeyword valueParser tagger
        |> checkWhitespaceFollows



-- PRIVATE


tagParser : String -> Parser a -> (a -> b) -> Parser b
tagParser tagKeyword valueParser tagger =
    P.succeed tagger
        |. P.token ("@" ++ tagKeyword ++ "(")
        |= valueParser
        |. P.token ")"
