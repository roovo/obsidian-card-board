module TagBoard exposing
    ( ColumnConfig
    , Config
    , columnConfigsParser
    , columns
    , configDecoder
    , configDecoder_v_0_1_0
    , configEncoder
    , defaultConfig
    )

import Filter exposing (Filter)
import List.Extra as LE
import Parser as P exposing ((|.), (|=), Parser)
import ParserHelper
import String.Extra as SE
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Config =
    { columns : List ColumnConfig
    , completedCount : Int
    , filters : List Filter
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
    , completedCount = 10
    , filters = []
    , includeOthers = False
    , includeUntagged = False
    , title = ""
    }



-- SERIALIZATION


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "columns" .columns <| TsEncode.list columnConfigEncoder
        , TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
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


configDecoder : TsDecode.Decoder Config
configDecoder =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_1_0 : TsDecode.Decoder Config
configDecoder_v_0_1_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "columns" (TsDecode.list columnConfigDecoder))
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.field "includeOthers" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUntagged" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


columnConfigDecoder : TsDecode.Decoder ColumnConfig
columnConfigDecoder =
    TsDecode.succeed ColumnConfig
        |> TsDecode.andMap (TsDecode.field "tag" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "displayTitle" TsDecode.string)



-- COLUMNS


columns : Config -> TaskList -> List ( String, List TaskItem )
columns config taskList =
    config.columns
        |> LE.uniqueBy .tag
        |> List.foldl (fillColumn taskList) []
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


appendCompleted : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
appendCompleted config taskList columnList =
    let
        completedTasks : List TaskItem
        completedTasks =
            taskList
                |> TaskList.filter isCompleteWithTags
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
        List.append columnList [ ( "Completed", completedTasks ) ]

    else
        columnList


prependOthers : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependOthers config taskList columnList =
    let
        cards : List TaskItem
        cards =
            taskList
                |> TaskList.filter isIncompleteWithoutTags
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
        ( "Others", cards ) :: columnList

    else
        columnList


prependUntagged : Config -> TaskList -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
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
        ( "Untagged", cards ) :: columnList

    else
        columnList


fillColumn : TaskList -> ColumnConfig -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
fillColumn taskList columnConfig acc =
    let
        isIncompleteWithTag : String -> TaskItem -> Bool
        isIncompleteWithTag tag item =
            not (TaskItem.isCompleted item) && TaskItem.hasTag tag item
    in
    TaskList.filter (isIncompleteWithTag columnConfig.tag) taskList
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Tuple.pair columnConfig.displayTitle
        |> List.singleton
        |> List.append acc
