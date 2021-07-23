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
    , title
    , toString
    , toggleCompletion
    )

import Date exposing (Date)
import Maybe.Extra as ME
import Parser exposing (..)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd, nonEmptyStringParser)



-- TYPES


type TaskItem
    = TaskItem String String Int Completion (Maybe Date) String


type Completion
    = Incomplete
    | Completed
    | CompletedOn Date


type Content
    = Word String
    | DoneTag Date
    | DueTag Date



-- INFO


title : TaskItem -> String
title (TaskItem _ _ _ _ _ t) =
    t


completion : TaskItem -> Completion
completion (TaskItem _ _ _ c _ _) =
    c


due : TaskItem -> Maybe Date
due (TaskItem _ _ _ _ d _) =
    d


filePath : TaskItem -> String
filePath (TaskItem _ p _ _ _ _) =
    p


id : TaskItem -> String
id (TaskItem _ p l _ _ _) =
    p ++ ":" ++ String.fromInt l


isDated : TaskItem -> Bool
isDated taskItem =
    taskItem
        |> due
        |> ME.isJust


isCompleted : TaskItem -> Bool
isCompleted (TaskItem _ _ _ c _ _) =
    case c of
        Incomplete ->
            False

        Completed ->
            True

        CompletedOn _ ->
            True


isFromFile : String -> TaskItem -> Bool
isFromFile pathToFile (TaskItem _ p _ _ _ _) =
    p == pathToFile


lineNumber : TaskItem -> Int
lineNumber (TaskItem _ _ l _ _ _) =
    l


originalText : TaskItem -> String
originalText (TaskItem s _ _ _ _ _) =
    s


toString : TaskItem -> String
toString (TaskItem _ _ _ c _ t) =
    case c of
        Incomplete ->
            "- [ ] " ++ String.trim t

        Completed ->
            "- [x] " ++ String.trim t

        CompletedOn completionDate ->
            "- [x] " ++ String.trim t ++ " @done(" ++ Date.toIsoString completionDate ++ ")"



-- MODIFICATION


toggleCompletion : Maybe Date -> TaskItem -> TaskItem
toggleCompletion completionDate (TaskItem o p l c d t) =
    case ( c, completionDate ) of
        ( Completed, _ ) ->
            TaskItem o p l Incomplete d t

        ( CompletedOn _, _ ) ->
            TaskItem o p l Incomplete d t

        ( Incomplete, Nothing ) ->
            TaskItem o p l Completed d t

        ( Incomplete, Just date ) ->
            TaskItem o p l (CompletedOn date) d t


markCompleted : TaskItem -> Date -> TaskItem
markCompleted (TaskItem o p l c d t) completionDate =
    TaskItem o p l (CompletedOn completionDate) d t



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
taskItemBuilder startOffset path row c dated contents endOffset source =
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

                DueTag completionDate ->
                    words

        dueDate =
            dated

        extractCompletionDate : Content -> Maybe Date -> Maybe Date
        extractCompletionDate content date =
            case content of
                Word word ->
                    date

                DoneTag completionDate ->
                    Just completionDate

                DueTag completionDate ->
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
        |> TaskItem sourceText path row c dueDate
        |> addCompletionDate


contentParser : Parser (List Content)
contentParser =
    loop [] contentHelp


contentHelp : List Content -> Parser (Step (List Content) (List Content))
contentHelp revContents =
    oneOf
        [ succeed (\content -> Loop (content :: revContents))
            |= wordOrDoneOrDueTagParser
            |. chompWhile isSpaceOrTab
        , succeed ()
            |> map (\_ -> Done (List.reverse revContents))
        ]


wordOrDoneOrDueTagParser : Parser Content
wordOrDoneOrDueTagParser =
    oneOf
        [ backtrackable
            doneTagParser
        , succeed Word
            |= ParserHelper.wordParser
        ]


doneTagParser : Parser Content
doneTagParser =
    succeed DoneTag
        |. token "@done("
        |= ParserHelper.dateParser
        |. token ")"


dueTagParser : Parser Content
dueTagParser =
    succeed DueTag
        |. token "@due("
        |= ParserHelper.dateParser
        |. token ")"


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
