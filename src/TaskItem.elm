module TaskItem exposing
    ( Completion(..)
    , TaskItem
    , completion
    , due
    , filePath
    , hasTags
    , id
    , isCompleted
    , isDated
    , isFromFile
    , lineNumber
    , originalText
    , parser
    , tags
    , title
    , toString
    , toggleCompletion
    )

import Date exposing (Date)
import Maybe.Extra as ME
import Parser exposing (..)
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
    = TaskItem TaskItemFields


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
title (TaskItem fields) =
    fields.title


completion : TaskItem -> Completion
completion (TaskItem fields) =
    fields.completion


due : TaskItem -> Maybe Date
due (TaskItem fields) =
    case fields.dueTag of
        Just _ ->
            fields.dueTag

        Nothing ->
            fields.dueFile


filePath : TaskItem -> String
filePath (TaskItem fields) =
    fields.filePath


hasTags : TaskItem -> Bool
hasTags (TaskItem fields) =
    List.length fields.tags /= 0


id : TaskItem -> String
id (TaskItem fields) =
    fields.filePath ++ ":" ++ String.fromInt fields.lineNumber


isDated : TaskItem -> Bool
isDated taskItem =
    taskItem
        |> due
        |> ME.isJust


isCompleted : TaskItem -> Bool
isCompleted (TaskItem fields) =
    case fields.completion of
        Incomplete ->
            False

        Completed ->
            True

        CompletedOn _ ->
            True


isFromFile : String -> TaskItem -> Bool
isFromFile pathToFile (TaskItem fields) =
    fields.filePath == pathToFile


lineNumber : TaskItem -> Int
lineNumber (TaskItem fields) =
    fields.lineNumber


originalText : TaskItem -> String
originalText (TaskItem fields) =
    fields.originalText


tags : TaskItem -> List String
tags (TaskItem fields) =
    fields.tags


toString : TaskItem -> String
toString (TaskItem fields) =
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
toggleCompletion completionDate (TaskItem fields) =
    case ( fields.completion, completionDate ) of
        ( Completed, _ ) ->
            TaskItem { fields | completion = Incomplete }

        ( CompletedOn _, _ ) ->
            TaskItem { fields | completion = Incomplete }

        ( Incomplete, Nothing ) ->
            TaskItem { fields | completion = Completed }

        ( Incomplete, Just date ) ->
            TaskItem { fields | completion = CompletedOn date }


markCompleted : TaskItem -> Date -> TaskItem
markCompleted (TaskItem fields) completionDate =
    TaskItem { fields | completion = CompletedOn completionDate }



-- SERIALIZATION


parser : String -> Maybe String -> Parser TaskItem
parser pathToFile fileDate =
    (succeed taskItemBuilder
        |= Parser.getOffset
        |= succeed pathToFile
        |= Parser.getRow
        |= prefixParser
        |. chompWhile isSpaceOrTab
        |= fileDateParser fileDate
        |= contentParser
        |= Parser.getOffset
        |. lineEndOrEnd
        |= Parser.getSource
    )
        |> andThen rejectIfNoTitle


taskItemBuilder : Int -> String -> Int -> Completion -> Maybe Date -> List Content -> Int -> String -> TaskItem
taskItemBuilder startOffset path row c dueFromFile contents endOffset source =
    let
        sourceText : String
        sourceText =
            String.slice startOffset endOffset source

        extractWords : Content -> List String -> List String
        extractWords content words =
            case content of
                Word word ->
                    word :: words

                DoneTag _ ->
                    words

                DueTag _ ->
                    words

                ObsidianTag _ ->
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
                Word _ ->
                    date

                DoneTag _ ->
                    date

                DueTag tagDate ->
                    Just tagDate

                ObsidianTag _ ->
                    date

        extractCompletionDate : Content -> Maybe Date -> Maybe Date
        extractCompletionDate content date =
            case content of
                Word _ ->
                    date

                DoneTag completionDate ->
                    Just completionDate

                DueTag _ ->
                    date

                ObsidianTag _ ->
                    date

        addCompletionDate : TaskItem -> TaskItem
        addCompletionDate item =
            if isCompleted item then
                contents
                    |> List.foldr extractCompletionDate Nothing
                    |> Maybe.map (markCompleted item)
                    |> Maybe.withDefault item

            else
                item

        parsedTitle : String
        parsedTitle =
            contents
                |> List.foldr extractWords []
                |> String.join " "
    in
    TaskItem
        { originalText = sourceText
        , filePath = path
        , lineNumber = row
        , completion = c
        , dueFile = dueFromFile
        , dueTag = tagDueDate
        , tags = obsidianTags
        , title = parsedTitle
        }
        |> addCompletionDate


contentParser : Parser (List Content)
contentParser =
    loop [] contentHelp


contentHelp : List Content -> Parser (Step (List Content) (List Content))
contentHelp revContents =
    oneOf
        [ succeed (\content -> Loop (content :: revContents))
            |= tokenParser
            |. chompWhile isSpaceOrTab
        , succeed ()
            |> map (\_ -> Done (List.reverse revContents))
        ]


tokenParser : Parser Content
tokenParser =
    oneOf
        [ backtrackable <| TaskPaperTag.doneTagParser DoneTag
        , backtrackable <| TaskPaperTag.dueTagParser DueTag
        , backtrackable <| obsidianTagParser
        , succeed Word
            |= ParserHelper.wordParser
        ]


obsidianTagParser : Parser Content
obsidianTagParser =
    succeed ObsidianTag
        |. token "#"
        |= ParserHelper.wordParser


prefixParser : Parser Completion
prefixParser =
    oneOf
        [ succeed Incomplete
            |. token "- [ ] "
        , succeed Completed
            |. token "- [x] "
        , succeed Completed
            |. token "- [X] "
        ]


fileDateParser : Maybe String -> Parser (Maybe Date)
fileDateParser fileDate =
    fileDate
        |> Maybe.map Date.fromIsoString
        |> Maybe.map Result.toMaybe
        |> ME.join
        |> succeed


rejectIfNoTitle : TaskItem -> Parser TaskItem
rejectIfNoTitle item =
    if String.length (title item) == 0 then
        problem "Task has no title"

    else
        succeed item
