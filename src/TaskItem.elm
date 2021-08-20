module TaskItem exposing
    ( Completion(..)
    , TaskItem
    , completion
    , due
    , filePath
    , hasSubtasks
    , hasTags
    , id
    , isCompleted
    , isDated
    , isFromFile
    , lineNumber
    , originalText
    , parser
    , subtasks
    , tags
    , title
    , toString
    , toggleCompletion
    )

import Date exposing (Date)
import Maybe.Extra as ME
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd, nonEmptyStringParser)
import TaskPaperTag



-- TYPES


type alias TaskItemFields =
    { originalText : String
    , filePath : String
    , lineNumber : Int
    , completion : Completion
    , dueFile : Maybe Date
    , dueTag : Maybe Date
    , tags : List String
    , title : String
    }


type TaskItem
    = TaskItem TaskItemFields (List TaskItemFields)


type Completion
    = Incomplete
    | Completed
    | CompletedOn Date


type Content
    = Word String
    | DoneTag Date
    | DueTag Date
    | ObsidianTag String



-- INFO


title : TaskItem -> String
title (TaskItem fields _) =
    fields.title


completion : TaskItem -> Completion
completion (TaskItem fields _) =
    fields.completion


due : TaskItem -> Maybe Date
due (TaskItem fields _) =
    case fields.dueTag of
        Just _ ->
            fields.dueTag

        Nothing ->
            fields.dueFile


filePath : TaskItem -> String
filePath (TaskItem fields _) =
    fields.filePath


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


originalText : TaskItem -> String
originalText (TaskItem fields _) =
    fields.originalText


subtasks : TaskItem -> List TaskItem
subtasks (TaskItem fields subtasks_) =
    List.map (\s -> TaskItem s []) subtasks_


tags : TaskItem -> List String
tags (TaskItem fields _) =
    fields.tags


toString : TaskItem -> String
toString (TaskItem fields _) =
    let
        checkbox =
            case fields.completion of
                Incomplete ->
                    "- [ ] "

                _ ->
                    "- [x] "

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
    in
    checkbox ++ String.trim fields.title ++ dueTag ++ completionTag



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


markCompleted : TaskItem -> Date -> TaskItem
markCompleted (TaskItem fields subtasks_) completionDate =
    TaskItem { fields | completion = CompletedOn completionDate } subtasks_



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
        |> P.andThen (addAnySubtasks pathToFile fileDate)


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
            contents
                |> List.foldr extractWords []
                |> String.join " "
    in
    { originalText = sourceText
    , filePath = path
    , lineNumber = row
    , completion = completion_
    , dueFile = dueFromFile
    , dueTag = tagDueDate
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


addAnySubtasks : String -> Maybe String -> TaskItemFields -> Parser TaskItem
addAnySubtasks pathToFile fileDate fields =
    P.succeed (TaskItem fields)
        |= ParserHelper.indentParser (subTaskParser pathToFile fileDate)


subTaskParser : String -> Maybe String -> Parser TaskItemFields
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


rejectIfNoTitle : TaskItemFields -> Parser TaskItemFields
rejectIfNoTitle fields =
    if String.length fields.title == 0 then
        P.problem "Task has no title"

    else
        P.succeed fields
