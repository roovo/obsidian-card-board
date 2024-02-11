module TaskList exposing
    ( TaskList
    , add
    , append
    , concat
    , containsTask
    , decoder
    , empty
    , encoder
    , filter
    , foldl
    , fromList
    , fromMarkdown
    , map
    , markdownDiffs
    , replaceTaskItems
    , taskContainingId
    , taskFromId
    , taskIds
    , taskTitles
    , toList
    , topLevelTasks
    )

import AssocSet exposing (Set)
import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import List.Extra as LE
import MarkdownFile exposing (MarkdownFile)
import Parser as P exposing (Parser)
import ParserHelper exposing (anyLineParser)
import TagList exposing (TagList)
import TaskItem exposing (TaskItem)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type TaskList
    = TaskList (List TaskItem)


type alias TaskListDiff =
    { toAdd : List TaskItem
    , toDelete : List TaskItem
    , toUpdate : List ( String, TaskItem )
    }



-- CREATE


empty : TaskList
empty =
    TaskList []


fromList : List TaskItem -> TaskList
fromList =
    TaskList


fromMarkdown : DataviewTaskCompletion -> MarkdownFile -> TaskList
fromMarkdown dataviewTaskCompletion markdownFile =
    P.run
        (parser
            dataviewTaskCompletion
            markdownFile.filePath
            markdownFile.fileDate
            markdownFile.frontMatterTags
            markdownFile.bodyOffset
        )
        (markdownFile.body ++ "\n")
        |> Result.withDefault empty


add : TaskItem -> TaskList -> TaskList
add item (TaskList list) =
    TaskList (item :: list)



-- SERIALIZE


decoder : TsDecode.Decoder TaskList
decoder =
    TsDecode.map TaskList (TsDecode.list TaskItem.decoder)


encoder : TsEncode.Encoder TaskList
encoder =
    TsEncode.map topLevelTasks (TsEncode.list TaskItem.encoder)



-- COMBINE


append : TaskList -> TaskList -> TaskList
append (TaskList l1) (TaskList l2) =
    TaskList <| List.append l1 l2


concat : List TaskList -> TaskList
concat =
    List.foldr append empty



-- TRANSFORM


filter : (TaskItem -> Bool) -> TaskList -> TaskList
filter fn =
    TaskList << List.filter fn << topLevelTasks


foldl : (TaskItem -> b -> b) -> b -> TaskList -> b
foldl fn acc =
    List.foldl fn acc << topLevelTasks


map : (TaskItem -> TaskItem) -> TaskList -> TaskList
map fn =
    TaskList << List.map fn << topLevelTasks


replaceTaskItems : List ( String, TaskItem ) -> TaskList -> TaskList
replaceTaskItems replacementDetails taskList =
    let
        idsToRemove : List String
        idsToRemove =
            List.map Tuple.first replacementDetails

        taskListToAdd : TaskList
        taskListToAdd =
            TaskList <| List.map Tuple.second replacementDetails
    in
    taskList
        |> filter (\i -> not <| List.member (TaskItem.id i) idsToRemove)
        |> append taskListToAdd



-- UTILITIES


containsTask : String -> TaskList -> Bool
containsTask taskId taskList =
    taskList
        |> topLevelTasks
        |> List.any (\ti -> TaskItem.id ti == taskId)


markdownDiffs : DataviewTaskCompletion -> MarkdownFile -> TaskList -> TaskListDiff
markdownDiffs dataviewTaskCompletion updatedMarkdown taskList =
    let
        updatedTasks : Set ( String, TaskItem )
        updatedTasks =
            fromMarkdown dataviewTaskCompletion updatedMarkdown
                |> toList
                |> List.map (\i -> ( TaskItem.id i, i ))
                |> AssocSet.fromList

        existingTasks : Set ( String, TaskItem )
        existingTasks =
            filter (TaskItem.isFromFile updatedMarkdown.filePath) taskList
                |> toList
                |> List.map (\i -> ( TaskItem.id i, i ))
                |> AssocSet.fromList

        toAdd : List TaskItem
        toAdd =
            AssocSet.diff updatedTasks existingTasks
                |> AssocSet.toList
                |> List.map Tuple.second

        toRemove : List TaskItem
        toRemove =
            AssocSet.diff existingTasks updatedTasks
                |> AssocSet.toList
                |> List.map Tuple.second
    in
    TaskListDiff toAdd toRemove []


taskTitles : TaskList -> List String
taskTitles =
    List.map TaskItem.title << topLevelTasks


taskIds : TaskList -> List String
taskIds =
    List.map TaskItem.id << topLevelTasks


taskContainingId : String -> TaskList -> Maybe TaskItem
taskContainingId id =
    LE.find (TaskItem.containsId id) << toList


taskFromId : String -> TaskList -> Maybe TaskItem
taskFromId id =
    LE.find (\i -> TaskItem.id i == id) << toList


toList : TaskList -> List TaskItem
toList =
    List.concatMap (\t -> t :: TaskItem.descendantTasks t) << topLevelTasks


topLevelTasks : TaskList -> List TaskItem
topLevelTasks (TaskList taskList) =
    taskList



-- PRIVATE


itemsNotFromFile : String -> List TaskItem -> List TaskItem
itemsNotFromFile pathToFile =
    List.filter (\t -> not (TaskItem.isFromFile pathToFile t))


parser : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> Parser TaskList
parser dataviewTaskCompletion filePath fileDate frontMatterTags bodyOffset =
    P.loop [] (taskItemsHelp dataviewTaskCompletion filePath fileDate frontMatterTags bodyOffset)
        |> P.map (\ts -> TaskList ts)


taskItemsHelp : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> List TaskItem -> Parser (P.Step (List TaskItem) (List TaskItem))
taskItemsHelp dataviewTaskCompletion filePath fileDate frontMatterTags bodyOffset revTaskItems =
    P.oneOf
        [ P.backtrackable
            (TaskItem.parser dataviewTaskCompletion filePath fileDate frontMatterTags bodyOffset
                |> P.map (\taskItem -> P.Loop (taskItem :: revTaskItems))
            )
        , anyLineParser
            |> P.map (\_ -> P.Loop revTaskItems)
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revTaskItems))
        ]
