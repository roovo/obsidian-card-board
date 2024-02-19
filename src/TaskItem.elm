module TaskItem exposing
    ( AutoCompletion(..)
    , CompareResult(..)
    , Completion(..)
    , Content
    , TaskItem
    , TaskItemFields
    , allSubtasksWithMatchingTagCompleted
    , asSingleTaskItems
    , compare
    , completedPosix
    , completion
    , containsId
    , decoder
    , descendantTasks
    , due
    , dueRataDie
    , dummy
    , encoder
    , fields
    , filePath
    , hasNotes
    , hasSubtasks
    , hasTags
    , hasThisTag
    , hasTopLevelTags
    , id
    , isAllowed
    , isCompleted
    , isDated
    , isFromFile
    , lineNumber
    , notes
    , originalBlock
    , originalLine
    , parser
    , removeFileNameDate
    , removeMatchingTags
    , removeTags
    , tags
    , tasksToToggle
    , title
    , titleWithTags
    , topLevelTags
    , updateDueDate
    , updateFilePath
    )

import DataviewDate
import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import Date exposing (Date)
import DecodeHelpers
import DueDate exposing (DueDate)
import EncodeHelpers
import FNV1a
import Filter exposing (Filter, Scope)
import List.Extra as LE
import Maybe.Extra as ME
import ObsidianTasksDate
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper exposing (isSpaceOrTab, lineEndOrEnd)
import Tag exposing (Tag)
import TagList exposing (TagList)
import TaskPaperTag
import Time
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias TaskItemFields =
    { autoComplete : AutoCompletion
    , completion : Completion
    , contents : List Content
    , dueFile : Maybe Date
    , dueTag : DueDate
    , filePath : String
    , lineNumber : Int
    , notes : String
    , originalBlock : String
    , originalLine : String
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
    | DueTag DueDate
    | ObsidianTag Tag
    | Word String


type IndentedItem
    = Subtask TaskItemFields
    | Note String


type CompareResult
    = Different
    | Identical
    | Moved
    | MovedAndUpdated
    | Updated


dummy : TaskItem
dummy =
    TaskItem defaultFields []


defaultFields : TaskItemFields
defaultFields =
    { autoComplete = NotSpecifed
    , completion = Incomplete
    , dueFile = Nothing
    , dueTag = DueDate.NotSet
    , filePath = ""
    , lineNumber = 0
    , notes = ""
    , originalBlock = ""
    , originalLine = ""
    , tags = TagList.empty
    , title = []
    , contents = []
    }



-- ENCODE / DECODE


decoder : TsDecode.Decoder TaskItem
decoder =
    TsDecode.succeed TaskItem
        |> TsDecode.andMap (TsDecode.field "fields" taskItemFieldsDecoder)
        |> TsDecode.andMap (TsDecode.field "subFields" (TsDecode.list taskItemFieldsDecoder))


encoder : TsEncode.Encoder TaskItem
encoder =
    let
        foo : TaskItem -> { fields : TaskItemFields, subFields : List TaskItemFields }
        foo (TaskItem fields_ subFields_) =
            { fields = fields_
            , subFields = subFields_
            }
    in
    TsEncode.map foo
        (TsEncode.object
            [ TsEncode.required "fields" .fields taskItemFieldsEncoder
            , TsEncode.required "subFields" .subFields (TsEncode.list taskItemFieldsEncoder)
            ]
        )



-- INFO


allSubtasksWithMatchingTagCompleted : String -> TaskItem -> Bool
allSubtasksWithMatchingTagCompleted tagToMatch taskItem =
    let
        subtasksWithMatchingTag : List TaskItem
        subtasksWithMatchingTag =
            taskItem
                |> descendantTasks
                |> List.filter (hasThisTag tagToMatch)
    in
    case subtasksWithMatchingTag of
        [] ->
            False

        matchingSubtasks ->
            List.all isCompleted matchingSubtasks


asSingleTaskItems : TaskItem -> List TaskItem
asSingleTaskItems ((TaskItem fields_ _) as taskItem) =
    TaskItem fields_ [] :: descendantTasks taskItem


completedPosix : TaskItem -> Int
completedPosix ((TaskItem _ subtasks_) as taskItem) =
    let
        mostRecentCompletedSubtaskMillis : Int
        mostRecentCompletedSubtaskMillis =
            subtasks_
                |> List.map posix
                |> List.maximum
                |> Maybe.withDefault 0

        posix : TaskItemFields -> Int
        posix f =
            case f.completion of
                CompletedAt time_ ->
                    Time.posixToMillis time_

                _ ->
                    0

        topLevelCompletion : Int
        topLevelCompletion =
            posix (fields taskItem)
    in
    if topLevelCompletion > 0 then
        topLevelCompletion

    else
        mostRecentCompletedSubtaskMillis


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
        DueDate.SetToDate date ->
            Just date

        DueDate.SetToNone ->
            Nothing

        DueDate.NotSet ->
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


hasTopLevelTags : TaskItem -> Bool
hasTopLevelTags =
    not << TagList.isEmpty << topLevelTags


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


isAllowed : Scope -> TaskItem -> Filter -> Bool
isAllowed scope taskItem filter =
    case filter of
        Filter.FileFilter filePath_ ->
            isFromFile filePath_ taskItem

        Filter.PathFilter path ->
            fileIsFromPath (filePath taskItem) path

        Filter.TagFilter tag ->
            case scope of
                Filter.TopLevelOnly ->
                    topLevelTaskHasThisTag tag taskItem

                Filter.SubTasksOnly ->
                    descendantTaskHasThisTag tag taskItem

                Filter.Both ->
                    hasThisTag tag taskItem


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


originalBlock : TaskItem -> String
originalBlock =
    .originalBlock << fields


originalLine : TaskItem -> String
originalLine =
    .originalLine << fields


tags : TaskItem -> TagList
tags ((TaskItem fields_ _) as taskItem) =
    descendantTasks taskItem
        |> List.map (fields >> .tags)
        |> List.foldl TagList.append TagList.empty
        |> TagList.append fields_.tags
        |> TagList.unique
        |> TagList.sort


topLevelTags : TaskItem -> TagList
topLevelTags taskItem =
    taskItem
        |> fields
        |> .tags
        |> TagList.unique
        |> TagList.sort


tasksToToggle : String -> { a | time : Time.Posix } -> TaskItem -> List TaskItem
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


titleWithTags : TaskItem -> String
titleWithTags taskItem =
    let
        buildContents : List Content -> List String
        buildContents =
            List.foldl extractContent []

        extractContent : Content -> List String -> List String
        extractContent content strings =
            case content of
                Word word ->
                    word :: strings

                ObsidianTag tag ->
                    ("#" ++ Tag.toString tag) :: strings

                _ ->
                    strings
    in
    taskItem
        |> fields
        |> .contents
        |> buildContents
        |> String.join " "



-- COMPARE


compare : TaskItem -> TaskItem -> CompareResult
compare other this =
    let
        sameBlock : TaskItem -> TaskItem -> Bool
        sameBlock other_ this_ =
            originalBlock this == originalBlock other

        sameLine : TaskItem -> TaskItem -> Bool
        sameLine other_ this_ =
            originalLine this == originalLine other

        samePlace : TaskItem -> TaskItem -> Bool
        samePlace other_ this_ =
            id this == id other

        sameTitle : TaskItem -> TaskItem -> Bool
        sameTitle other_ this_ =
            title this == title other
    in
    if samePlace other this && sameBlock other this then
        Identical

    else if samePlace other this && sameTitle other this then
        Updated

    else if not (samePlace other this) && sameBlock other this then
        Moved

    else if not (samePlace other this) && sameTitle other this then
        MovedAndUpdated

    else
        Different



-- MODIFICATION


removeFileNameDate : TaskItem -> TaskItem
removeFileNameDate (TaskItem fields_ subtasks_) =
    TaskItem { fields_ | dueFile = Nothing } subtasks_


removeMatchingTags : String -> TaskItem -> TaskItem
removeMatchingTags tagToRemove taskItem =
    let
        remover : TagList -> TagList
        remover tagList =
            TagList.filter (not << Tag.matches tagToRemove) tagList
    in
    mapTags remover taskItem


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


updateDueDate : Maybe Date -> TaskItem -> TaskItem
updateDueDate date (TaskItem fields_ subtasks_) =
    case date of
        Just dueDate ->
            TaskItem { fields_ | dueTag = DueDate.SetToDate dueDate } subtasks_

        Nothing ->
            TaskItem { fields_ | dueTag = DueDate.SetToNone } subtasks_


updateFilePath : String -> String -> TaskItem -> TaskItem
updateFilePath oldPath newPath ((TaskItem fields_ subtasks_) as taskItem) =
    if fields_.filePath == oldPath then
        TaskItem { fields_ | filePath = newPath } subtasks_

    else
        taskItem



-- SERIALIZE


parser : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> Parser TaskItem
parser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset =
    P.succeed itemWithOriginalBlock
        |= P.getOffset
        |= taskItemParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset
        |= P.getOffset
        |= P.getSource



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


fileIsFromPath : String -> String -> Bool
fileIsFromPath file path =
    let
        pathComponents : List String
        pathComponents =
            path
                |> String.replace "\\" "/"
                |> String.split "/"
                |> List.filter (not << String.isEmpty)

        filePathComponents : List String
        filePathComponents =
            file
                |> String.replace "\\" "/"
                |> String.split "/"
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> List.filter (not << String.isEmpty)

        isComponentMatching : Int -> String -> Bool
        isComponentMatching index pathComponent =
            case LE.getAt index filePathComponents of
                Nothing ->
                    False

                Just filePathComponent ->
                    filePathComponent == pathComponent
    in
    pathComponents
        |> List.indexedMap isComponentMatching
        |> List.all identity


indentedItemParser : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> Parser IndentedItem
indentedItemParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset =
    P.oneOf
        [ subTaskParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset
        , notesParser
        ]


itemWithOriginalBlock : Int -> TaskItem -> Int -> String -> TaskItem
itemWithOriginalBlock startOffset (TaskItem fields_ subtasks_) endOffset source =
    TaskItem
        { fields_ | originalBlock = String.trimRight <| String.slice startOffset (endOffset - 0) source }
        subtasks_


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
        , P.succeed Incomplete
            |. P.token "+ [ ] "
        , P.succeed Completed
            |. P.token "+ [x] "
        , P.succeed Completed
            |. P.token "+ [X] "
        , P.succeed Incomplete
            |. P.token "* [ ] "
        , P.succeed Completed
            |. P.token "* [x] "
        , P.succeed Completed
            |. P.token "* [X] "
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

                DueTag dueDate ->
                    { fields_ | dueTag = dueDate }

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
        |> (\tif -> { tif | originalLine = sourceText })
        |> (\tif -> { tif | tags = TagList.append tif.tags frontMatterTags })
        |> (\tif -> { tif | title = List.reverse tif.title })
        |> (\tif -> { tif | contents = List.reverse contents })
        |> addCompletionTime


taskItemParser : DataviewTaskCompletion -> String -> Maybe String -> TagList -> Int -> Parser TaskItem
taskItemParser dataviewTaskCompletion pathToFile fileDate frontMatterTags bodyOffset =
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


toggleCompletion : { a | time : Time.Posix } -> TaskItem -> TaskItem
toggleCompletion timeWithZone (TaskItem fields_ subtasks_) =
    case fields_.completion of
        Completed ->
            TaskItem { fields_ | completion = Incomplete } subtasks_

        CompletedAt _ ->
            TaskItem { fields_ | completion = Incomplete } subtasks_

        Incomplete ->
            TaskItem { fields_ | completion = CompletedAt timeWithZone.time } subtasks_


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



-- PRIVATE


autoCompletionDecoder : TsDecode.Decoder AutoCompletion
autoCompletionDecoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant0 "FalseSpecified" FalseSpecified
        , DecodeHelpers.toElmVariant0 "NotSpecifed" NotSpecifed
        , DecodeHelpers.toElmVariant0 "TrueSpecified" TrueSpecified
        ]


autoCompletionEncoder : TsEncode.Encoder AutoCompletion
autoCompletionEncoder =
    TsEncode.union
        (\vFalseSpecified vNotSpecifed vTrueSpecified value ->
            case value of
                FalseSpecified ->
                    vFalseSpecified

                NotSpecifed ->
                    vNotSpecifed

                TrueSpecified ->
                    vTrueSpecified
        )
        |> TsEncode.variant0 "FalseSpecified"
        |> TsEncode.variant0 "NotSpecifed"
        |> TsEncode.variant0 "TrueSpecified"
        |> TsEncode.buildUnion


completionDecoder : TsDecode.Decoder Completion
completionDecoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant0 "Completed" Completed
        , DecodeHelpers.toElmVariant "CompletedAt" CompletedAt (TsDecode.map Time.millisToPosix TsDecode.int)
        , DecodeHelpers.toElmVariant0 "Incomplete" Incomplete
        ]


completionEncoder : TsEncode.Encoder Completion
completionEncoder =
    TsEncode.union
        (\vCompleted vCompletedAt vIncomplete value ->
            case value of
                Completed ->
                    vCompleted

                CompletedAt timeStamp ->
                    vCompletedAt timeStamp

                Incomplete ->
                    vIncomplete
        )
        |> TsEncode.variant0 "Completed"
        |> TsEncode.variantTagged "CompletedAt" (TsEncode.map Time.posixToMillis TsEncode.int)
        |> TsEncode.variant0 "Incomplete"
        |> TsEncode.buildUnion


contentDecoder : TsDecode.Decoder Content
contentDecoder =
    TsDecode.oneOf
        [ DecodeHelpers.toElmVariant "AutoCompleteTag" AutoCompleteTag autoCompletionDecoder
        , DecodeHelpers.toElmVariant "CompletedTag" CompletedTag (TsDecode.map Time.millisToPosix TsDecode.int)
        , DecodeHelpers.toElmVariant "DueTag" DueTag DueDate.decoder
        , DecodeHelpers.toElmVariant "ObsidianTag" ObsidianTag Tag.decoder
        , DecodeHelpers.toElmVariant "Word" Word TsDecode.string
        ]


contentEncoder : TsEncode.Encoder Content
contentEncoder =
    TsEncode.union
        (\vAutoCompleteTag vCompletedTag vDueTag vObsidianTag vWord value ->
            case value of
                AutoCompleteTag autoCompletion ->
                    vAutoCompleteTag autoCompletion

                CompletedTag timeStamp ->
                    vCompletedTag timeStamp

                DueTag date ->
                    vDueTag date

                ObsidianTag tag ->
                    vObsidianTag tag

                Word word ->
                    vWord word
        )
        |> TsEncode.variantTagged "AutoCompleteTag" autoCompletionEncoder
        |> TsEncode.variantTagged "CompletedTag" (TsEncode.map Time.posixToMillis TsEncode.int)
        |> TsEncode.variantTagged "DueTag" DueDate.encoder
        |> TsEncode.variantTagged "ObsidianTag" Tag.encoder
        |> TsEncode.variantTagged "Word" TsEncode.string
        |> TsEncode.buildUnion


descendantTaskHasThisTag : String -> TaskItem -> Bool
descendantTaskHasThisTag tagToMatch =
    let
        descendantTasksTags : TaskItem -> TagList
        descendantTasksTags taskItem =
            descendantTasks taskItem
                |> List.map (fields >> .tags)
                |> List.foldl TagList.append TagList.empty
                |> TagList.unique
                |> TagList.sort
    in
    TagList.containsTagMatching tagToMatch << descendantTasksTags


mapFields : (TaskItemFields -> TaskItemFields) -> TaskItem -> TaskItem
mapFields fn (TaskItem fields_ subtasks_) =
    TaskItem (fn fields_) subtasks_


mapTags : (TagList -> TagList) -> TaskItem -> TaskItem
mapTags fn taskItem =
    mapFields (\fs -> { fs | tags = fn fs.tags }) taskItem


taskItemFieldsDecoder : TsDecode.Decoder TaskItemFields
taskItemFieldsDecoder =
    TsDecode.succeed TaskItemFields
        |> TsDecode.andMap (TsDecode.field "autoComplete" autoCompletionDecoder)
        |> TsDecode.andMap (TsDecode.field "completion" completionDecoder)
        |> TsDecode.andMap (TsDecode.field "contents" (TsDecode.list contentDecoder))
        |> TsDecode.andMap (TsDecode.field "dueFile" (TsDecode.maybe DecodeHelpers.dateDecoder))
        |> TsDecode.andMap (TsDecode.field "dueTag" DueDate.decoder)
        |> TsDecode.andMap (TsDecode.field "filePath" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "lineNumber" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "notes" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "originalBlock" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "originalLine" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "tags" TagList.decoder)
        |> TsDecode.andMap (TsDecode.field "title" (TsDecode.list TsDecode.string))


taskItemFieldsEncoder : TsEncode.Encoder TaskItemFields
taskItemFieldsEncoder =
    TsEncode.object
        [ TsEncode.required "autoComplete" .autoComplete autoCompletionEncoder
        , TsEncode.required "completion" .completion completionEncoder
        , TsEncode.required "contents" .contents (TsEncode.list contentEncoder)
        , TsEncode.required "dueFile" .dueFile (TsEncode.maybe EncodeHelpers.dateEncoder)
        , TsEncode.required "dueTag" .dueTag DueDate.encoder
        , TsEncode.required "filePath" .filePath TsEncode.string
        , TsEncode.required "lineNumber" .lineNumber TsEncode.int
        , TsEncode.required "notes" .notes TsEncode.string
        , TsEncode.required "originalBlock" .originalBlock TsEncode.string
        , TsEncode.required "originalLine" .originalLine TsEncode.string
        , TsEncode.required "tags" .tags TagList.encoder
        , TsEncode.required "title" .title (TsEncode.list TsEncode.string)
        ]


topLevelTaskHasThisTag : String -> TaskItem -> Bool
topLevelTaskHasThisTag tagToMatch =
    let
        topLevelTasksTags : TaskItem -> TagList
        topLevelTasksTags (TaskItem fields_ _) =
            fields_.tags
    in
    TagList.containsTagMatching tagToMatch << topLevelTasksTags
