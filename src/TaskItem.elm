module TaskItem exposing
    ( AutoCompletion(..)
    , Completion(..)
    , TaskItem
    , TaskItemFields
    , completedPosix
    , completion
    , containsId
    , descendantTasks
    , due
    , dueRataDie
    , dummy
    , fields
    , filePath
    , hasNotes
    , hasOneOfTheTags
    , hasSubtasks
    , hasTags
    , hasThisTag
    , id
    , isCompleted
    , isDated
    , isFromFile
    , lineNumber
    , notes
    , originalText
    , parser
    , removeTags
    , tags
    , tasksToToggle
    , title
    , toToggledString
    , updateFilePath
    )

import DataviewDate
import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import Date exposing (Date)
import FNV1a
import GlobalSettings exposing (TaskCompletionFormat)
import Iso8601
import Maybe.Extra as ME
import ObsidianTasksDate
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd)
import Regex exposing (Regex)
import String.Extra as SE
import Tag exposing (Tag)
import TagList exposing (TagList)
import TaskPaperTag
import Time



-- TYPES


type alias TaskItemFields =
    { autoComplete : AutoCompletion
    , completion : Completion
    , dueFile : Maybe Date
    , dueTag : Maybe Date
    , filePath : String
    , lineNumber : Int
    , notes : String
    , originalText : String
    , tags : TagList
    , title : List String
    }


type TaskItem
    = TaskItem TaskItemFields (List TaskItemFields)


type AutoCompletion
    = NotSpecifed
    | FalseSpecified
    | TrueSpecified


type Completion
    = Incomplete
    | Completed
    | CompletedAt Time.Posix


type Content
    = AutoCompleteTag AutoCompletion
    | CompletedTag Time.Posix
    | DueTag Date
    | ObsidianTag Tag
    | Word String


type IndentedItem
    = Subtask TaskItemFields
    | Note String


dummy : TaskItem
dummy =
    TaskItem defaultFields []


defaultFields : TaskItemFields
defaultFields =
    { autoComplete = NotSpecifed
    , completion = Incomplete
    , dueFile = Nothing
    , dueTag = Nothing
    , filePath = ""
    , lineNumber = 0
    , notes = ""
    , originalText = ""
    , tags = TagList.empty
    , title = []
    }



-- INFO


completedPosix : TaskItem -> Int
completedPosix taskItem =
    case completion taskItem of
        CompletedAt time_ ->
            Time.posixToMillis time_

        _ ->
            0


completion : TaskItem -> Completion
completion =
    fields >> .completion


containsId : String -> TaskItem -> Bool
containsId targetId taskItem =
    (id taskItem :: List.map id (descendantTasks taskItem))
        |> List.member targetId


descendantTasks : TaskItem -> List TaskItem
descendantTasks (TaskItem _ subtasks_) =
    List.map (\s -> TaskItem s []) subtasks_


due : TaskItem -> Maybe Date
due (TaskItem fields_ _) =
    case fields_.dueTag of
        Just _ ->
            fields_.dueTag

        Nothing ->
            fields_.dueFile


dueRataDie : TaskItem -> Int
dueRataDie taskItem =
    case due taskItem of
        Just dueDate ->
            Date.toRataDie dueDate

        Nothing ->
            0


fields : TaskItem -> TaskItemFields
fields (TaskItem f _) =
    f


filePath : TaskItem -> String
filePath =
    .filePath << fields


hasNotes : TaskItem -> Bool
hasNotes =
    not << String.isEmpty << .notes << fields


hasTags : TaskItem -> Bool
hasTags =
    not << TagList.isEmpty << tags


hasOneOfTheTags : List String -> TaskItem -> Bool
hasOneOfTheTags tagsToMatch taskItem =
    List.any (\t -> hasThisTag t taskItem) tagsToMatch


hasThisTag : String -> TaskItem -> Bool
hasThisTag tagToMatch =
    TagList.containsTagMatching tagToMatch << tags


hasSubtasks : TaskItem -> Bool
hasSubtasks (TaskItem _ subtasks_) =
    not <| List.isEmpty subtasks_


id : TaskItem -> String
id (TaskItem fields_ _) =
    String.fromInt (FNV1a.hash fields_.filePath)
        ++ ":"
        ++ String.fromInt fields_.lineNumber


isCompleted : TaskItem -> Bool
isCompleted (TaskItem fields_ _) =
    case fields_.completion of
        Incomplete ->
            False

        _ ->
            True


isDated : TaskItem -> Bool
isDated =
    ME.isJust << due


isFromFile : String -> TaskItem -> Bool
isFromFile pathToFile =
    (==) pathToFile << .filePath << fields


lineNumber : TaskItem -> Int
lineNumber =
    .lineNumber << fields


notes : TaskItem -> String
notes =
    .notes << fields


originalText : TaskItem -> String
originalText =
    .originalText << fields


tags : TaskItem -> TagList
tags ((TaskItem fields_ _) as taskItem) =
    descendantTasks taskItem
        |> List.map (fields >> .tags)
        |> List.foldl TagList.append TagList.empty
        |> TagList.append fields_.tags
        |> TagList.unique
        |> TagList.sort


tasksToToggle : String -> { a | now : Time.Posix } -> TaskItem -> List TaskItem
tasksToToggle id_ timeWithZone taskItem =
    let
        autoComplete : TaskItem -> AutoCompletion
        autoComplete =
            fields >> .autoComplete

        idBelongsToSubtask : TaskItem -> Bool
        idBelongsToSubtask =
            List.member id_ << List.map id << descendantTasks

        matchingTaskItem : List TaskItem
        matchingTaskItem =
            List.filter (\t -> id t == id_) (taskItem :: descendantTasks taskItem)

        resultIsAllSubtasksCompleted : TaskItem -> Bool
        resultIsAllSubtasksCompleted =
            List.all isCompleted << List.map toggleIfMatches << descendantTasks

        shouldAutoComplete : TaskItem -> Bool
        shouldAutoComplete t =
            case autoComplete t of
                TrueSpecified ->
                    True

                _ ->
                    False

        toggleIfMatches : TaskItem -> TaskItem
        toggleIfMatches t =
            if id t == id_ then
                toggleCompletion timeWithZone t

            else
                t

        topLevelTaskIsNotAlreadyComplete : TaskItem -> Bool
        topLevelTaskIsNotAlreadyComplete =
            not << isCompleted
    in
    if
        idBelongsToSubtask taskItem
            && shouldAutoComplete taskItem
            && resultIsAllSubtasksCompleted taskItem
            && topLevelTaskIsNotAlreadyComplete taskItem
    then
        taskItem :: matchingTaskItem

    else
        matchingTaskItem


title : TaskItem -> String
title =
    String.join " " << .title << fields



-- MODIFICATION


removeTags : List String -> TaskItem -> TaskItem
removeTags toRemove (TaskItem fields_ subtasks_) =
    let
        isKeeper : Tag -> Bool
        isKeeper tag =
            List.all (\tagString -> not <| Tag.equals tagString tag) toRemove

        removeFromFields : TaskItemFields -> TaskItemFields
        removeFromFields f =
            { f | tags = TagList.filter isKeeper f.tags }
    in
    TaskItem
        (removeFromFields fields_)
        (List.map removeFromFields subtasks_)


updateFilePath : String -> String -> TaskItem -> TaskItem
updateFilePath oldPath newPath ((TaskItem fields_ subtasks_) as taskItem) =
    if fields_.filePath == oldPath then
        TaskItem { fields_ | filePath = newPath } subtasks_

    else
        taskItem



-- SERIALIZE


parser : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> Parser TaskItem
parser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset =
    (P.succeed taskItemFieldsBuilder
        |= P.getOffset
        |= P.getCol
        |= P.succeed pathToFile
        |= P.succeed frontMatterTags
        |= P.succeed bodyOffset
        |= P.getRow
        |= prefixParser
        |. P.chompWhile isSpaceOrTab
        |= fileDateParser fileDate
        |= contentParser dataviewTaskCompletion
        |= P.getOffset
        |. lineEndOrEnd
        |= P.getSource
    )
        |> P.andThen rejectIfNoTitle
        |> P.andThen (addAnySubtasksAndNotes dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset)



-- CONVERT


toToggledString : DataviewTaskCompletion -> TaskCompletionFormat -> { a | now : Time.Posix } -> TaskItem -> String
toToggledString dataviewTaskCompletion taskCompletionFormat timeWithZone ((TaskItem fields_ _) as taskItem) =
    let
        blockLinkRegex : Regex
        blockLinkRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "(\\s\\^[a-zA-Z\\d-]+)$"

        checkboxRegex : Regex
        checkboxRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "- \\[[ xX]\\]"

        completionTag : TaskItem -> String
        completionTag t_ =
            case completion t_ of
                Incomplete ->
                    let
                        completionString : String
                        completionString =
                            timeWithZone.now
                                |> Iso8601.fromTime
                                |> String.left 19
                    in
                    case taskCompletionFormat of
                        GlobalSettings.NoCompletion ->
                            ""

                        GlobalSettings.ObsidianCardBoard ->
                            " @completed(" ++ completionString ++ ")"

                        GlobalSettings.ObsidianDataview ->
                            case dataviewTaskCompletion of
                                DataviewTaskCompletion.NoCompletion ->
                                    ""

                                DataviewTaskCompletion.Emoji ->
                                    " ✅ " ++ String.left 10 completionString

                                DataviewTaskCompletion.Text t ->
                                    " [" ++ t ++ ":: " ++ String.left 10 completionString ++ "]"

                        GlobalSettings.ObsidianTasks ->
                            " ✅ " ++ String.left 10 completionString

                _ ->
                    ""

        insertCompletionTag : TaskItem -> String -> String
        insertCompletionTag t_ taskString =
            let
                tagInserter : String -> String
                tagInserter =
                    Regex.find blockLinkRegex taskString
                        |> List.head
                        |> Maybe.map .index
                        |> Maybe.withDefault (String.length taskString)
                        |> SE.insertAt (completionTag t_)
            in
            tagInserter taskString

        regexReplacer : String -> (Regex.Match -> String) -> String -> String
        regexReplacer regex replacer original =
            case Regex.fromString regex of
                Just r ->
                    Regex.replace r replacer original

                Nothing ->
                    original

        removeCompletionTags : String -> String
        removeCompletionTags =
            let
                dataviewRemover =
                    case dataviewTaskCompletion of
                        DataviewTaskCompletion.NoCompletion ->
                            identity

                        DataviewTaskCompletion.Emoji ->
                            identity

                        DataviewTaskCompletion.Text t ->
                            regexReplacer (" \\[" ++ t ++ ":: \\d{4}-\\d{2}-\\d{2}\\]") (\_ -> "")
            in
            regexReplacer " @completed\\(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\)" (\_ -> "")
                >> regexReplacer " ✅ \\d{4}-\\d{2}-\\d{2}" (\_ -> "")
                >> dataviewRemover

        replaceCheckbox : TaskItem -> String -> String
        replaceCheckbox t_ taskString =
            let
                checkboxIndex : Int
                checkboxIndex =
                    Regex.find checkboxRegex taskString
                        |> List.head
                        |> Maybe.map .index
                        |> Maybe.withDefault 0
            in
            SE.replaceSlice
                (toggledCheckbox t_)
                checkboxIndex
                (checkboxIndex + 5)
                taskString

        toggledCheckbox : TaskItem -> String
        toggledCheckbox t_ =
            case completion t_ of
                Incomplete ->
                    "- [x]"

                _ ->
                    "- [ ]"
    in
    fields_.originalText
        |> replaceCheckbox taskItem
        |> removeCompletionTags
        |> insertCompletionTag taskItem



-- PRIVATE


addAnySubtasksAndNotes : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> TaskItemFields -> Parser TaskItem
addAnySubtasksAndNotes dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset fields_ =
    let
        buildTaskItem : List IndentedItem -> Parser TaskItem
        buildTaskItem indentedItems =
            P.succeed (TaskItem { fields_ | notes = parsedNotes indentedItems } <| parsedSubtasks indentedItems)

        parsedSubtasks : List IndentedItem -> List TaskItemFields
        parsedSubtasks indentedItems =
            indentedItems
                |> List.filterMap
                    (\i ->
                        case i of
                            Subtask taskItemFields ->
                                Just taskItemFields

                            _ ->
                                Nothing
                    )

        parsedNotes : List IndentedItem -> String
        parsedNotes indentedItems =
            indentedItems
                |> List.filterMap
                    (\i ->
                        case i of
                            Note notes_ ->
                                Just notes_

                            _ ->
                                Nothing
                    )
                |> String.join "\n"
    in
    P.succeed identity
        |= ParserHelper.indentParser (indentedItemParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset)
        |> P.andThen buildTaskItem


autoCompleteTagger : Bool -> Content
autoCompleteTagger flag =
    if flag then
        AutoCompleteTag TrueSpecified

    else
        AutoCompleteTag FalseSpecified


contentHelp : DataviewTaskCompletion -> List Content -> Parser (P.Step (List Content) (List Content))
contentHelp dataviewTaskCompletion revContents =
    P.oneOf
        [ P.succeed (\content -> P.Loop (content :: revContents))
            |= tokenParser dataviewTaskCompletion
            |. P.chompWhile isSpaceOrTab
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revContents))
        ]


contentParser : DataviewTaskCompletion -> Parser (List Content)
contentParser dataviewTaskCompletion =
    P.loop [] <| contentHelp dataviewTaskCompletion


fileDateParser : Maybe String -> Parser (Maybe Date)
fileDateParser fileDate =
    fileDate
        |> Maybe.map Date.fromIsoString
        |> Maybe.map Result.toMaybe
        |> ME.join
        |> P.succeed


indentedItemParser : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> Parser IndentedItem
indentedItemParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset =
    P.oneOf
        [ subTaskParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset
        , notesParser
        ]


notesParser : Parser IndentedItem
notesParser =
    P.succeed Note
        |= ParserHelper.anyLineParser


prefixParser : Parser Completion
prefixParser =
    P.oneOf
        [ P.succeed Incomplete
            |. P.token "- [ ] "
        , P.succeed Completed
            |. P.token "- [x] "
        , P.succeed Completed
            |. P.token "- [X] "
        ]


rejectIfNoTitle : TaskItemFields -> Parser TaskItemFields
rejectIfNoTitle fields_ =
    if List.isEmpty fields_.title then
        P.problem "Task has no title"

    else
        P.succeed fields_


subTaskParser : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> Parser IndentedItem
subTaskParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset =
    P.succeed taskItemFieldsBuilder
        |= P.getOffset
        |= P.getCol
        |= P.succeed pathToFile
        |= P.succeed frontMatterTags
        |= P.succeed bodyOffset
        |= P.getRow
        |= prefixParser
        |. P.chompWhile isSpaceOrTab
        |= fileDateParser fileDate
        |= contentParser dataviewTaskCompletion
        |= P.getOffset
        |. lineEndOrEnd
        |= P.getSource
        |> P.andThen (\f -> P.succeed (Subtask f))


taskItemFieldsBuilder : Int -> Int -> String -> TagList -> Int -> Int -> Completion -> Maybe Date -> List Content -> Int -> String -> TaskItemFields
taskItemFieldsBuilder startOffset startColumn path frontMatterTags bodyOffset row completion_ dueFromFile contents endOffset source =
    let
        sourceText : String
        sourceText =
            String.slice (startOffset - (startColumn - 1)) endOffset source

        extractCompletionTime : Content -> Maybe Time.Posix -> Maybe Time.Posix
        extractCompletionTime content time =
            case content of
                CompletedTag completionTime ->
                    Just completionTime

                _ ->
                    time

        addCompletionTime : TaskItemFields -> TaskItemFields
        addCompletionTime fields_ =
            if isCompleted (TaskItem fields_ []) then
                contents
                    |> List.foldr extractCompletionTime Nothing
                    |> Maybe.map (\completionDate_ -> { fields_ | completion = CompletedAt completionDate_ })
                    |> Maybe.withDefault fields_

            else
                fields_

        extractContents : Content -> TaskItemFields -> TaskItemFields
        extractContents content fields_ =
            case content of
                Word word ->
                    { fields_ | title = word :: fields_.title }

                DueTag date ->
                    { fields_ | dueTag = Just date }

                CompletedTag _ ->
                    fields_

                ObsidianTag tag ->
                    { fields_ | tags = TagList.cons tag fields_.tags }

                AutoCompleteTag tag ->
                    { fields_ | autoComplete = tag }

        removeTrailingBlockLink : List Content -> List Content
        removeTrailingBlockLink contents_ =
            case List.reverse contents_ of
                (Word endWord) :: cs ->
                    if String.startsWith "^" endWord then
                        List.reverse cs

                    else
                        contents

                _ ->
                    contents
    in
    contents
        |> removeTrailingBlockLink
        |> List.foldl extractContents defaultFields
        |> (\tif -> { tif | completion = completion_ })
        |> (\tif -> { tif | dueFile = dueFromFile })
        |> (\tif -> { tif | filePath = path })
        |> (\tif -> { tif | lineNumber = bodyOffset + row })
        |> (\tif -> { tif | originalText = sourceText })
        |> (\tif -> { tif | tags = TagList.append tif.tags frontMatterTags })
        |> (\tif -> { tif | title = List.reverse tif.title })
        |> addCompletionTime


toggleCompletion : { a | now : Time.Posix } -> TaskItem -> TaskItem
toggleCompletion timeWithZone (TaskItem fields_ subtasks_) =
    case fields_.completion of
        Completed ->
            TaskItem { fields_ | completion = Incomplete } subtasks_

        CompletedAt _ ->
            TaskItem { fields_ | completion = Incomplete } subtasks_

        Incomplete ->
            TaskItem { fields_ | completion = CompletedAt timeWithZone.now } subtasks_


tokenParser : DataviewTaskCompletion -> Parser Content
tokenParser dataviewTaskCompletion =
    P.oneOf
        [ P.backtrackable <| TaskPaperTag.completedTagParser CompletedTag
        , P.backtrackable <| TaskPaperTag.dueTagParser DueTag
        , P.backtrackable <| TaskPaperTag.autocompleteTagParser autoCompleteTagger
        , P.backtrackable <| ObsidianTasksDate.dueDateParser DueTag
        , P.backtrackable <| ObsidianTasksDate.completionTimeParser CompletedTag
        , P.backtrackable <| DataviewDate.dueDateParser DueTag
        , P.backtrackable <| DataviewDate.completionTimeParser dataviewTaskCompletion CompletedTag
        , P.backtrackable <| P.map ObsidianTag Tag.parser
        , P.succeed Word
            |= ParserHelper.wordParser
        ]
