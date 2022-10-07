module TagBoard exposing
    ( ColumnConfig
    , Config
    , columnConfigsParser
    , columns
    , configDecoder_v_0_1_0
    , configDecoder_v_0_2_0
    , configDecoder_v_0_3_0
    , configDecoder_v_0_4_0
    , configEncoder
    , defaultConfig
    )

import Column exposing (Column)
import Filter exposing (Filter, Polarity)
import List.Extra as LE
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import String.Extra as SE
import Tag exposing (Tag)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Config =
    { columns : List ColumnConfig
    , showColumnTags : Bool
    , completedCount : Int
    , filters : List Filter
    , filterPolarity : Polarity
    , showFilteredTags : Bool
    , includeOthers : Bool
    , includeUntagged : Bool
    , title : String
    }


type alias ColumnConfig =
    { tag : String
    , displayTitle : String
    }


defaultConfig : Config
defaultConfig =
    { columns = []
    , showColumnTags = True
    , completedCount = 10
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , showFilteredTags = True
    , includeOthers = False
    , includeUntagged = False
    , title = ""
    }



-- SERIALIZATION


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "columns" .columns <| TsEncode.list columnConfigEncoder
        , TsEncode.required "showColumnTags" .showColumnTags TsEncode.bool
        , TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "includeOthers" .includeOthers TsEncode.bool
        , TsEncode.required "includeUntagged" .includeUntagged TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        ]


columnConfigEncoder : TsEncode.Encoder ColumnConfig
columnConfigEncoder =
    TsEncode.object
        [ TsEncode.required "tag" .tag TsEncode.string
        , TsEncode.required "displayTitle" .displayTitle TsEncode.string
        ]


configDecoder_v_0_4_0 : TsDecode.Decoder Config
configDecoder_v_0_4_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.field "showColumnTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_3_0 : TsDecode.Decoder Config
configDecoder_v_0_3_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_2_0 : TsDecode.Decoder Config
configDecoder_v_0_2_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_1_0 : TsDecode.Decoder Config
configDecoder_v_0_1_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Filter.Allow)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


columnConfigDecoder : TsDecode.Decoder ColumnConfig
columnConfigDecoder =
    TsDecode.succeed ColumnConfig
        |> TsDecode.andMap (TsDecode.field "tag" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "displayTitle" TsDecode.string)



-- COLUMNS


columns : Config -> TaskList -> List (Column TaskItem)
columns config taskList =
    config.columns
        |> LE.uniqueBy .tag
        |> List.foldl (fillColumn taskList config) []
        |> prependOthers config taskList
        |> prependUntagged config taskList
        |> appendCompleted config taskList



-- PARSING


columnConfigsParser : Parser (List ColumnConfig)
columnConfigsParser =
    P.loop [] columnConfigHelp


columnConfigHelp : List ColumnConfig -> Parser (P.Step (List ColumnConfig) (List ColumnConfig))
columnConfigHelp revStmts =
    P.oneOf
        [ P.succeed (\stmt -> P.Loop (stmt :: revStmts))
            |= columnConfigParser
            |. P.spaces
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse revStmts))
        ]


columnConfigParser : Parser ColumnConfig
columnConfigParser =
    let
        buildColumnConfig : ( String, Maybe String ) -> Parser ColumnConfig
        buildColumnConfig ( tag, title ) =
            let
                cleanedTag : String
                cleanedTag =
                    if String.startsWith "#" tag then
                        String.dropLeft 1 tag

                    else
                        tag

                displayTitle : String
                displayTitle =
                    title
                        |> Maybe.withDefault defaultTitle
                        |> String.words
                        |> String.join " "

                defaultTitle : String
                defaultTitle =
                    cleanedTag
                        |> String.replace "/" " "
                        |> SE.toSentenceCase
            in
            P.succeed { tag = cleanedTag, displayTitle = displayTitle }
    in
    P.succeed Tuple.pair
        |. ParserHelper.spaces
        |= ParserHelper.wordParser
        |. ParserHelper.spaces
        |= P.oneOf
            [ P.map Just ParserHelper.nonEmptyStringParser
            , P.succeed Nothing
            ]
        |> P.andThen buildColumnConfig



-- PRIVATE


taskListWithTagsRemoved : Config -> TaskList -> TaskList
taskListWithTagsRemoved config taskList =
    let
        columnTags : List String
        columnTags =
            List.map .tag config.columns

        filterTags : List String
        filterTags =
            config.filters
                |> List.filter (\f -> Filter.filterType f == "Tags")
                |> List.map Filter.value

        tagsToRemove : List String
        tagsToRemove =
            case ( config.showFilteredTags, config.showColumnTags ) of
                ( True, True ) ->
                    []

                ( False, True ) ->
                    filterTags

                ( True, False ) ->
                    columnTags

                ( False, False ) ->
                    filterTags ++ columnTags
    in
    TaskList.removeTags tagsToRemove taskList


appendCompleted : Config -> TaskList -> List (Column TaskItem) -> List (Column TaskItem)
appendCompleted config taskList columnList =
    let
        completedTasks : List TaskItem
        completedTasks =
            taskList
                |> TaskList.filter isCompleteWithTags
                |> taskListWithTagsRemoved config
                |> TaskList.topLevelTasks
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.reverse
                |> List.sortBy TaskItem.completedPosix
                |> List.reverse
                |> List.take config.completedCount

        isCompleteWithTags : TaskItem -> Bool
        isCompleteWithTags item =
            TaskItem.isCompleted item && TaskItem.hasOneOfTheTags uniqueColumnTags item

        uniqueColumnTags : List String
        uniqueColumnTags =
            config.columns
                |> LE.uniqueBy .tag
                |> List.map .tag
    in
    if config.completedCount > 0 then
        List.append columnList [ Column.init "Completed" completedTasks ]

    else
        columnList


prependOthers : Config -> TaskList -> List (Column TaskItem) -> List (Column TaskItem)
prependOthers config taskList columnList =
    let
        cards : List TaskItem
        cards =
            taskList
                |> TaskList.filter isIncompleteWithoutTags
                |> taskListWithTagsRemoved config
                |> TaskList.topLevelTasks
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.sortBy TaskItem.dueRataDie

        isIncompleteWithoutTags : TaskItem -> Bool
        isIncompleteWithoutTags item =
            not (TaskItem.isCompleted item) && TaskItem.hasTags item && not (TaskItem.hasOneOfTheTags uniqueColumnTags item)

        uniqueColumnTags : List String
        uniqueColumnTags =
            config.columns
                |> LE.uniqueBy .tag
                |> List.map .tag
    in
    if config.includeOthers then
        Column.init "Others" cards :: columnList

    else
        columnList


prependUntagged : Config -> TaskList -> List (Column TaskItem) -> List (Column TaskItem)
prependUntagged config taskList columnList =
    let
        cards : List TaskItem
        cards =
            taskList
                |> TaskList.filter isIncompleteWithNoTags
                |> TaskList.topLevelTasks
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.sortBy TaskItem.dueRataDie

        isIncompleteWithNoTags : TaskItem -> Bool
        isIncompleteWithNoTags item =
            not (TaskItem.isCompleted item) && not (TaskItem.hasTags item)
    in
    if config.includeUntagged then
        Column.init "Untagged" cards :: columnList

    else
        columnList


fillColumn : TaskList -> Config -> ColumnConfig -> List (Column TaskItem) -> List (Column TaskItem)
fillColumn taskList config columnConfig acc =
    let
        isIncompleteWithTag : String -> TaskItem -> Bool
        isIncompleteWithTag tag item =
            not (TaskItem.isCompleted item) && TaskItem.hasThisTag tag item
    in
    TaskList.filter (isIncompleteWithTag columnConfig.tag) taskList
        |> taskListWithTagsRemoved config
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init columnConfig.displayTitle
        |> List.singleton
        |> List.append acc
