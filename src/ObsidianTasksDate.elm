module ObsidianTasksDate exposing (ObsidianTasksDate(..), parser)

import Date exposing (Date)
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper



-- TYPES


type ObsidianTasksDate
    = Due Date
    | Scheduled Date
    | Completed Date



-- PARSE


parser : Parser ObsidianTasksDate
parser =
    P.oneOf
        [ P.backtrackable <| builder "📅 " Due
        , P.backtrackable <| builder "✅ " Completed
        , P.backtrackable <| builder "⏳ " Scheduled
        ]



-- PRIVATE


builder : String -> (Date -> ObsidianTasksDate) -> Parser ObsidianTasksDate
builder emoticon tagger =
    P.succeed tagger
        |. P.token emoticon
        |= ParserHelper.dateParser
