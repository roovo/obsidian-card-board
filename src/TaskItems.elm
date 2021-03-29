module TaskItems exposing
    ( TaskList
    , build
    , parser
    , taskItems
    )

import Date exposing (Date)
import Parser exposing (..)
import ParserHelper exposing (anyLineParser)
import TaskItem exposing (Dated(..), TaskItem)



-- TYPES


type TaskList
    = TaskList (List TaskItem)


taskItems : TaskList -> List TaskItem
taskItems (TaskList l) =
    l


build : List TaskItem -> TaskList
build source =
    TaskList source



-- PARSEING


parser : String -> Maybe String -> Parser (List TaskItem)
parser filePath fileDate =
    loop [] (taskItemsHelp filePath fileDate)



-- PRIVATE


taskItemsHelp : String -> Maybe String -> List TaskItem -> Parser (Step (List TaskItem) (List TaskItem))
taskItemsHelp filePath fileDate revTaskItems =
    oneOf
        [ backtrackable
            (TaskItem.parser filePath fileDate
                |> map (\taskItem -> Loop (taskItem :: revTaskItems))
            )
        , anyLineParser
            |> map (\_ -> Loop revTaskItems)
        , succeed ()
            |> map (\_ -> Done (List.reverse revTaskItems))
        ]
