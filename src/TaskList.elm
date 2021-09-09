module TaskList exposing
    ( TaskList
    , append
    , concat
    , empty
    , fromMarkdown
    , parser
    , removeForFile
    , replaceForFile
    , taskContainingId
    , taskFromId
    , taskIds
    , taskTitles
    , tasks
    , topLevelTasks
    )

import Date exposing (Date)
import List.Extra as LE
import Parser exposing (..)
import ParserHelper exposing (anyLineParser)
import TaskItem exposing (TaskItem)



-- TYPES


type TaskList
    = TaskList (List TaskItem)


empty : TaskList
empty =
    TaskList []



-- PARSING


parser : String -> Maybe String -> Parser TaskList
parser filePath fileDate =
    loop [] (taskItemsHelp filePath fileDate)
        |> map (\ts -> TaskList ts)


fromMarkdown : String -> Maybe String -> String -> TaskList
fromMarkdown filePath fileDate fileContents =
    Parser.run (parser filePath fileDate) (fileContents ++ "\n")
        |> Result.withDefault empty



-- COMBINE


append : TaskList -> TaskList -> TaskList
append (TaskList root) (TaskList toAppend) =
    TaskList <| root ++ toAppend


concat : List TaskList -> TaskList
concat =
    List.foldr append empty



-- MANIPULATE


replaceForFile : String -> TaskList -> TaskList -> TaskList
replaceForFile filePath updatedList currentList =
    currentList
        |> removeForFile filePath
        |> append updatedList


removeForFile : String -> TaskList -> TaskList
removeForFile filePath (TaskList taskItems) =
    taskItems
        |> itemsNotFromFile filePath
        |> TaskList



-- INFO


taskTitles : TaskList -> List String
taskTitles (TaskList taskItems) =
    taskItems
        |> List.map TaskItem.title


taskIds : TaskList -> List String
taskIds (TaskList taskItems) =
    taskItems
        |> List.map TaskItem.id


taskContainingId : String -> TaskList -> Maybe TaskItem
taskContainingId id taskList =
    taskList
        |> tasks
        |> LE.find (TaskItem.containsId id)


taskFromId : String -> TaskList -> Maybe TaskItem
taskFromId id taskList =
    taskList
        |> tasks
        |> LE.find (\i -> TaskItem.id i == id)


tasks : TaskList -> List TaskItem
tasks (TaskList taskList) =
    taskList
        |> List.concatMap
            (\t -> t :: TaskItem.subtasks t)


topLevelTasks : TaskList -> List TaskItem
topLevelTasks (TaskList taskList) =
    taskList



-- PRIVATE


itemsNotFromFile : String -> List TaskItem -> List TaskItem
itemsNotFromFile pathToFile taskItems =
    taskItems
        |> List.filter (\t -> not (TaskItem.isFromFile pathToFile t))


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
