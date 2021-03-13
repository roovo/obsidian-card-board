module TaskItems exposing (parser)

import Parser exposing (..)
import ParserHelper exposing (anyLineParser)
import TaskItem exposing (TaskItem)


parser : Parser (List TaskItem)
parser =
    loop [] taskItemsHelp


taskItemsHelp : List TaskItem -> Parser (Step (List TaskItem) (List TaskItem))
taskItemsHelp revTaskItems =
    oneOf
        [ TaskItem.parser
            |> map (\taskItem -> Loop (taskItem :: revTaskItems))
        , anyLineParser
            |> map (\_ -> Loop revTaskItems)
        , succeed ()
            |> map (\_ -> Done (List.reverse revTaskItems))
        ]
