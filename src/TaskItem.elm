module TaskItem exposing
    ( AutoCompletion(..)
    , Completion(..)
    , TaskItem
    , autoComplete
    , blockLink
    , completedRataDie
    , completion
    , containsId
    , due
    , dueRataDie
    , filePath
    , hasNotes
    , hasSubtasks
    , hasTags
    , id
    , isCompleted
    , isDated
    , isFromFile
    , lineNumber
    , notes
    , originalText
    , parser
    , subtasks
    , tags
    , tasksToToggle
    , title
    , toString
    , toggleCompletion
    )

import Date exposing (Date)
import List.Extra as LE
import Maybe.Extra as ME
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd)
import TaskPaperTag



-- TYPES


type alias TaskItemFields =
    { autoComplete : AutoCompletion
    , blockLink : Maybe String
    , completion : Completion
    , dueFile : Maybe Date
    , dueTag : Maybe Date
    , filePath : String
    , lineNumber : Int
    , notes : String
    , originalText : String
    , tags : List String
    , title : String
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
    | CompletedOn Date


type Content
    = AutoCompleteTag Bool
    | DoneTag Date
    | DueTag Date
    | ObsidianTag String
    | Word String


type IndentedItem
    = Subtask TaskItemFields
    | Note String



-- INFO


autoComplete : TaskItem -> AutoCompletion
autoComplete (TaskItem fields _) =
    fields.autoComplete


blockLink : TaskItem -> Maybe String
blockLink (TaskItem fields _) =
    fields.blockLink


completedRataDie : TaskItem -> Int
completedRataDie taskItem =
    case completion taskItem of
        CompletedOn date_ ->
            Date.toRataDie date_

        _ ->
            0


completion : TaskItem -> Completion
completion (TaskItem fields _) =
    fields.completion


containsId : String -> TaskItem -> Bool
containsId targetId taskItem =
    (id taskItem :: List.map id (subtasks taskItem))
        |> List.member targetId


due : TaskItem -> Maybe Date
due (TaskItem fields _) =
    case fields.dueTag of
        Just _ ->
            fields.dueTag

        Nothing ->
            fields.dueFile


dueRataDie : TaskItem -> Int
dueRataDie taskItem =
    case due taskItem of
        Just dueDate ->
            Date.toRataDie dueDate

        Nothing ->
            0


filePath : TaskItem -> String
filePath (TaskItem fields _) =
    fields.filePath


hasNotes : TaskItem -> Bool
hasNotes (TaskItem fields _) =
    not <| String.isEmpty fields.notes


hasTags : TaskItem -> Bool
hasTags (TaskItem fields _) =
    not <| List.isEmpty fields.tags


hasSubtasks : TaskItem -> Bool
hasSubtasks (TaskItem _ subtasks_) =
    not <| List.isEmpty subtasks_


id : TaskItem -> String
id (TaskItem fields _) =
    fields.filePath ++ ":" ++ String.fromInt fields.lineNumber


isDated : TaskItem -> Bool
isDated taskItem =
    taskItem
        |> due
        |> ME.isJust


isCompleted : TaskItem -> Bool
isCompleted (TaskItem fields _) =
    case fields.completion of
        Incomplete ->
            False

        Completed ->
            True

        CompletedOn _ ->
            True


isFromFile : String -> TaskItem -> Bool
isFromFile pathToFile (TaskItem fields _) =
    fields.filePath == pathToFile


lineNumber : TaskItem -> Int
lineNumber (TaskItem fields _) =
    fields.lineNumber


notes : TaskItem -> String
notes (TaskItem fields _) =
    fields.notes


originalText : TaskItem -> String
originalText (TaskItem fields _) =
    fields.originalText


subtasks : TaskItem -> List TaskItem
subtasks (TaskItem _ subtasks_) =
    List.map (\s -> TaskItem s []) subtasks_


tags : TaskItem -> List String
tags ((TaskItem fields _) as taskItem) =
    subtasks taskItem
        |> List.concatMap (\(TaskItem fs _) -> fs.tags)
        |> List.append fields.tags


tasksToToggle : String -> TaskItem -> List TaskItem
tasksToToggle id_ taskItem =
    let
        idBelongsToSubtask =
            taskItem
                |> subtasks
                |> List.map id
                |> List.member id_

        resultIsAllSubtasksCompleted =
            subtasks taskItem
                |> List.map
                    (\t ->
                        if id t == id_ then
                            toggleCompletion Nothing t

                        else
                            t
                    )
                |> List.all isCompleted

        matchingTaskItem =
            (taskItem :: subtasks taskItem)
                |> List.filter (\t -> id t == id_)

        topLevelTaskIsNotAlreadyComplete =
            not <| isCompleted taskItem

        shouldAutoComplete =
            case autoComplete taskItem of
                TrueSpecified ->
                    True

                _ ->
                    False
    in
    if shouldAutoComplete && idBelongsToSubtask && resultIsAllSubtasksCompleted && topLevelTaskIsNotAlreadyComplete then
        taskItem :: matchingTaskItem

    else
        matchingTaskItem


title : TaskItem -> String
title (TaskItem fields _) =
    fields.title


toString : TaskItem -> String
toString (TaskItem fields _) =
    let
        leadingWhiteSpace =
            fields.originalText
                |> String.toList
                |> LE.takeWhile ParserHelper.isSpaceOrTab
                |> String.fromList

        checkbox =
            case fields.completion of
                Incomplete ->
                    "- [ ] "

                _ ->
                    "- [x] "

        fieldTags =
            if List.length fields.tags > 0 then
                fields.tags
                    |> List.map (String.append "#")
                    |> String.join " "
                    |> String.append " "

            else
                ""

        dueTag =
            case fields.dueTag of
                Just date ->
                    " @due(" ++ Date.toIsoString date ++ ")"

                _ ->
                    ""

        completionTag =
            case fields.completion of
                CompletedOn completionDate ->
                    " @done(" ++ Date.toIsoString completionDate ++ ")"

                _ ->
                    ""

        autoCompleteTag =
            case fields.autoComplete of
                NotSpecifed ->
                    ""

                FalseSpecified ->
                    " @autodone(false)"

                TrueSpecified ->
                    " @autodone(true)"

        blockLinkText =
            case fields.blockLink of
                Just blockLink_ ->
                    " " ++ blockLink_

                _ ->
                    ""
    in
    leadingWhiteSpace ++ checkbox ++ String.trim fields.title ++ fieldTags ++ dueTag ++ autoCompleteTag ++ completionTag ++ blockLinkText



-- MODIFICATION


toggleCompletion : Maybe Date -> TaskItem -> TaskItem
toggleCompletion completionDate (TaskItem fields subtasks_) =
    case ( fields.completion, completionDate ) of
        ( Completed, _ ) ->
            TaskItem { fields | completion = Incomplete } subtasks_

        ( CompletedOn _, _ ) ->
            TaskItem { fields | completion = Incomplete } subtasks_

        ( Incomplete, Nothing ) ->
            TaskItem { fields | completion = Completed } subtasks_

        ( Incomplete, Just date ) ->
            TaskItem { fields | completion = CompletedOn date } subtasks_



-- SERIALIZATION


parser : String -> Maybe String -> Parser TaskItem
parser pathToFile fileDate =
    (P.succeed taskItemFieldsBuilder
        |= P.getOffset
        |= P.getCol
        |= P.succeed pathToFile
        |= P.getRow
        |= prefixParser
        |. P.chompWhile isSpaceOrTab
        |= fileDateParser fileDate
        |= contentParser
        |= P.getOffset
        |. lineEndOrEnd
        |= P.getSource
    )
        |> P.andThen rejectIfNoTitle
        |> P.andThen (addAnySubtasksAndNotes pathToFile fileDate)


taskItemFieldsBuilder : Int -> Int -> String -> Int -> Completion -> Maybe Date -> List Content -> Int -> String -> TaskItemFields
taskItemFieldsBuilder startOffset startColumn path row completion_ dueFromFile contents endOffset source =
    let
        sourceText : String
        sourceText =
            String.slice (startOffset - (startColumn - 1)) endOffset source

        extractWords : Content -> List String -> List String
        extractWords content words =
            case content of
                Word word ->
                    word :: words

                _ ->
                    words

        tagDueDate : Maybe Date
        tagDueDate =
            contents
                |> List.foldr extractDueDate Nothing

        obsidianTags : List String
        obsidianTags =
            contents
                |> List.foldr extractTag []

        extractTag : Content -> List String -> List String
        extractTag content ts =
            case content of
                ObsidianTag t ->
                    t :: ts

                _ ->
                    ts

        extractDueDate : Content -> Maybe Date -> Maybe Date
        extractDueDate content date =
            case content of
                DueTag tagDate ->
                    Just tagDate

                _ ->
                    date

        extractCompletionDate : Content -> Maybe Date -> Maybe Date
        extractCompletionDate content date =
            case content of
                DoneTag completionDate ->
                    Just completionDate

                _ ->
                    date

        extractAutoComplete : Content -> AutoCompletion -> AutoCompletion
        extractAutoComplete content autoComplete_ =
            case content of
                AutoCompleteTag False ->
                    FalseSpecified

                AutoCompleteTag True ->
                    TrueSpecified

                _ ->
                    autoComplete_

        autoCompletefromTag : AutoCompletion
        autoCompletefromTag =
            List.foldr extractAutoComplete NotSpecifed contents

        addCompletionDate : TaskItemFields -> TaskItemFields
        addCompletionDate fields =
            if isCompleted (TaskItem fields []) then
                contents
                    |> List.foldr extractCompletionDate Nothing
                    |> Maybe.map (\completionDate_ -> { fields | completion = CompletedOn completionDate_ })
                    |> Maybe.withDefault fields

            else
                fields

        parsedTitle : String
        parsedTitle =
            let
                wordsWithoutBlockLink : List Content
                wordsWithoutBlockLink =
                    case List.reverse contents of
                        (Word endWord) :: cs ->
                            if String.startsWith "^" endWord then
                                List.reverse cs

                            else
                                contents

                        _ ->
                            contents
            in
            wordsWithoutBlockLink
                |> List.foldr extractWords []
                |> String.join " "

        parsedBlockLink : Maybe String
        parsedBlockLink =
            case List.reverse contents of
                (Word endWord) :: _ ->
                    if String.startsWith "^" endWord then
                        Just endWord

                    else
                        Nothing

                _ ->
                    Nothing
    in
    { autoComplete = autoCompletefromTag
    , blockLink = parsedBlockLink
    , completion = completion_
    , dueFile = dueFromFile
    , dueTag = tagDueDate
    , filePath = path
    , lineNumber = row
    , notes = ""
    , originalText = sourceText
    , tags = obsidianTags
    , title = parsedTitle
    }
        |> addCompletionDate


contentParser : Parser (List Content)
contentParser =
    P.loop [] contentHelp


contentHelp : List Content -> Parser (P.Step (List Content) (List Content))
contentHelp revContents =
    P.oneOf
        [ P.succeed (\content -> P.Loop (content :: revContents))
            |= tokenParser
            |. P.chompWhile isSpaceOrTab
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revContents))
        ]


tokenParser : Parser Content
tokenParser =
    P.oneOf
        [ P.backtrackable <| TaskPaperTag.doneTagParser DoneTag
        , P.backtrackable <| TaskPaperTag.dueTagParser DueTag
        , P.backtrackable <| TaskPaperTag.autodoneTagParser AutoCompleteTag
        , P.backtrackable <| obsidianTagParser
        , P.succeed Word
            |= ParserHelper.wordParser
        ]


obsidianTagParser : Parser Content
obsidianTagParser =
    P.succeed ObsidianTag
        |. P.token "#"
        |= ParserHelper.wordParser


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


fileDateParser : Maybe String -> Parser (Maybe Date)
fileDateParser fileDate =
    fileDate
        |> Maybe.map Date.fromIsoString
        |> Maybe.map Result.toMaybe
        |> ME.join
        |> P.succeed


addAnySubtasksAndNotes : String -> Maybe String -> TaskItemFields -> Parser TaskItem
addAnySubtasksAndNotes pathToFile fileDate fields =
    let
        buildTaskItem : List IndentedItem -> Parser TaskItem
        buildTaskItem indentedItems =
            P.succeed (TaskItem { fields | notes = parsedNotes indentedItems } <| parsedSubtasks indentedItems)

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
        |= ParserHelper.indentParser (indentedItemParser pathToFile fileDate)
        |> P.andThen buildTaskItem


indentedItemParser : String -> Maybe String -> Parser IndentedItem
indentedItemParser pathToFile fileDate =
    P.oneOf
        [ subTaskParser pathToFile fileDate
        , notesParser
        ]


notesParser : Parser IndentedItem
notesParser =
    P.succeed Note
        |= ParserHelper.anyLineParser


subTaskParser : String -> Maybe String -> Parser IndentedItem
subTaskParser pathToFile fileDate =
    P.succeed taskItemFieldsBuilder
        |= P.getOffset
        |= P.getCol
        |= P.succeed pathToFile
        |= P.getRow
        |= prefixParser
        |. P.chompWhile isSpaceOrTab
        |= fileDateParser fileDate
        |= contentParser
        |= P.getOffset
        |. lineEndOrEnd
        |= P.getSource
        |> P.andThen (\f -> P.succeed (Subtask f))


rejectIfNoTitle : TaskItemFields -> Parser TaskItemFields
rejectIfNoTitle fields =
    if String.length fields.title == 0 then
        P.problem "Task has no title"

    else
        P.succeed fields
