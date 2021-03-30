module TaskList exposing
    ( TaskList
    , append
    , concat
    , empty
    , parser
    , replaceForFile
    , taskItems
    )

import Date exposing (Date)
import Parser exposing (..)
import ParserHelper exposing (anyLineParser)
import TaskItem exposing (Dated(..), TaskItem)



-- TO REMOVE??


taskItems : TaskList -> List TaskItem
taskItems (TaskList l) =
    l


concat : List TaskList -> TaskList
concat taskLists =
    taskLists
        |> List.map (\l -> taskItems l)
        |> List.concat
        |> TaskList



-- TYPES


type TaskList
    = TaskList (List TaskItem)


empty : TaskList
empty =
    TaskList []



-- COMBINE


append : TaskList -> TaskList -> TaskList
append (TaskList root) (TaskList toAppend) =
    TaskList <| root ++ toAppend



-- MANIPULATE


replaceForFile : String -> TaskList -> TaskList -> TaskList
replaceForFile filePath updatedList currentList =
    let
        modelWithTasksRemoved =
            removeForFile currentList filePath
    in
    append modelWithTasksRemoved updatedList



-- PARSING


parser : String -> Maybe String -> Parser TaskList
parser filePath fileDate =
    loop [] (taskItemsHelp filePath fileDate)
        |> map (\ts -> TaskList ts)



-- PRIVATE


removeForFile : TaskList -> String -> TaskList
removeForFile taskList filePath =
    let
        leftOverTaskItems =
            TaskItem.notFromFile filePath (taskItems taskList)
    in
    TaskList leftOverTaskItems


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
