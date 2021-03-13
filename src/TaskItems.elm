module TaskItems exposing (parser)

import Parser exposing (..)
import ParserHelper exposing (anyLineParser)
import TaskItem exposing (TaskItem)


parser : Maybe String -> Parser (List TaskItem)
parser fileDate =
    loop [] (taskItemsHelp fileDate)


taskItemsHelp : Maybe String -> List TaskItem -> Parser (Step (List TaskItem) (List TaskItem))
taskItemsHelp fileDate revTaskItems =
    oneOf
        [ TaskItem.parser fileDate
            |> map (\taskItem -> Loop (taskItem :: revTaskItems))
        , anyLineParser
            |> map (\_ -> Loop revTaskItems)
        , succeed ()
            |> map (\_ -> Done (List.reverse revTaskItems))
        ]
