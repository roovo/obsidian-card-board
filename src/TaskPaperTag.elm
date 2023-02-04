module TaskPaperTag exposing
    ( autocompleteTagParser
    , completedTagParser
    , dueTagParser
    , parser
    )

import DueDate exposing (DueDate)
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


dueTagParser : (DueDate -> a) -> Parser a
dueTagParser =
    P.oneOf
        [ P.map DueDate.SetToDate ParserHelper.dateParser
        , P.map (always DueDate.SetToNone) (P.token "none")
        ]
        |> parser "due"


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
