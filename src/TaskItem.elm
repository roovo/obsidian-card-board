module TaskItem exposing
    ( Completion(..)
    , TaskItem
    , completion
    , due
    , filePath
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


type TaskItem
    = TaskItem String String Int Completion (Maybe Date) (Maybe Date) (List String) String


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
title (TaskItem _ _ _ _ _ _ _ t) =
    t


completion : TaskItem -> Completion
completion (TaskItem _ _ _ c _ _ _ _) =
    c


due : TaskItem -> Maybe Date
due (TaskItem _ _ _ _ df dt _ _) =
    case dt of
        Just _ ->
            dt

        Nothing ->
            df


filePath : TaskItem -> String
filePath (TaskItem _ p _ _ _ _ _ _) =
    p


id : TaskItem -> String
id (TaskItem _ p l _ _ _ _ _) =
    p ++ ":" ++ String.fromInt l


isDated : TaskItem -> Bool
isDated taskItem =
    taskItem
        |> due
        |> ME.isJust


isCompleted : TaskItem -> Bool
isCompleted (TaskItem _ _ _ c _ _ _ _) =
    case c of
        Incomplete ->
            False

        Completed ->
            True

        CompletedOn _ ->
            True


isFromFile : String -> TaskItem -> Bool
isFromFile pathToFile (TaskItem _ p _ _ _ _ _ _) =
    p == pathToFile


lineNumber : TaskItem -> Int
lineNumber (TaskItem _ _ l _ _ _ _ _) =
    l


originalText : TaskItem -> String
originalText (TaskItem s _ _ _ _ _ _ _) =
    s


tags : TaskItem -> List String
tags (TaskItem _ _ _ _ _ _ ts _) =
    ts


toString : TaskItem -> String
toString (TaskItem _ _ _ c _ dt _ t) =
    let
        checkbox =
            case c of
                Incomplete ->
                    "- [ ] "

                _ ->
                    "- [x] "

        dueTag =
            case dt of
                Just date ->
                    " @due(" ++ Date.toIsoString date ++ ")"

                _ ->
                    ""

        completionTag =
            case c of
                CompletedOn completionDate ->
                    " @done(" ++ Date.toIsoString completionDate ++ ")"

                _ ->
                    ""
    in
    checkbox ++ String.trim t ++ dueTag ++ completionTag



-- MODIFICATION


toggleCompletion : Maybe Date -> TaskItem -> TaskItem
toggleCompletion completionDate (TaskItem o p l c df dt ts t) =
    case ( c, completionDate ) of
        ( Completed, _ ) ->
            TaskItem o p l Incomplete df dt ts t

        ( CompletedOn _, _ ) ->
            TaskItem o p l Incomplete df dt ts t

        ( Incomplete, Nothing ) ->
            TaskItem o p l Completed df dt ts t

        ( Incomplete, Just date ) ->
            TaskItem o p l (CompletedOn date) df dt ts t


markCompleted : TaskItem -> Date -> TaskItem
markCompleted (TaskItem o p l c df dt ts t) completionDate =
    TaskItem o p l (CompletedOn completionDate) df dt ts t



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
    in
    contents
        |> List.foldr extractWords []
        |> String.join " "
        |> TaskItem sourceText path row c dueFromFile tagDueDate obsidianTags
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
