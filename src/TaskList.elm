module TaskList exposing
    ( TaskList
    , append
    , completedItems
    , concat
    , empty
    , futureItems
    , parser
    , removeForFile
    , replaceForFile
    , taskTitles
    , todaysItems
    , tomorrowsItems
    , undatedItems
    )

import Date exposing (Date)
import Parser exposing (..)
import ParserHelper exposing (anyLineParser)
import TaskItem exposing (Dated(..), TaskItem)



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
    let
        listWithTasksRemoved =
            removeForFile filePath currentList
    in
    append updatedList listWithTasksRemoved


removeForFile : String -> TaskList -> TaskList
removeForFile filePath (TaskList taskItems) =
    let
        leftOverTaskItems =
            itemsNotFromFile filePath taskItems
    in
    TaskList leftOverTaskItems



-- INFO


taskTitles : TaskList -> List String
taskTitles (TaskList taskItems) =
    taskItems
        |> List.map TaskItem.title



-- FILTERS


undatedItems : TaskList -> List TaskItem
undatedItems (TaskList taskItems) =
    taskItems
        |> List.filter (\t -> (not <| TaskItem.isCompleted t) && (not <| TaskItem.isDated t))
        |> List.sortBy TaskItem.filePath


todaysItems : Date -> TaskList -> List TaskItem
todaysItems today (TaskList taskItems) =
    let
        isToday t =
            case TaskItem.due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days today date <= 0 then
                        True

                    else
                        False
    in
    taskItems
        |> List.filter (\t -> (not <| TaskItem.isCompleted t) && isToday t)


tomorrowsItems : Date -> TaskList -> List TaskItem
tomorrowsItems today (TaskList taskItems) =
    let
        tomorrow =
            Date.add Date.Days 1 today

        isTomorrow t =
            case TaskItem.due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days tomorrow date == 0 then
                        True

                    else
                        False
    in
    taskItems
        |> List.filter (\t -> isTomorrow t && (not <| TaskItem.isCompleted t))


futureItems : Date -> TaskList -> List TaskItem
futureItems today (TaskList taskItems) =
    let
        tomorrow =
            Date.add Date.Days 1 today

        isToday t =
            case TaskItem.due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days tomorrow date > 0 then
                        True

                    else
                        False
    in
    taskItems
        |> List.filter (\t -> (not <| TaskItem.isCompleted t) && isToday t)


completedItems : TaskList -> List TaskItem
completedItems (TaskList taskItems) =
    taskItems
        |> List.filter TaskItem.isCompleted



-- PRIVATE


itemsFromFile : String -> List TaskItem -> List TaskItem
itemsFromFile pathToFile =
    List.filter <| TaskItem.isFromFile pathToFile


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
