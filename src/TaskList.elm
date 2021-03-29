module TaskList exposing
    ( TaskList
    , build
    , concat
    , empty
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


empty : TaskList
empty =
    TaskList []


concat : List TaskList -> TaskList
concat taskLists =
    taskLists
        |> List.map (\l -> taskItems l)
        |> List.concat
        |> build



-- PARSEING


parser : String -> Maybe String -> Parser TaskList
parser filePath fileDate =
    loop [] (taskItemsHelp filePath fileDate)
        |> map (\ts -> TaskList ts)



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
