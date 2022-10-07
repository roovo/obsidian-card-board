module DateBoard exposing
    ( Config
    , columns
    , configDecoder_v_0_1_0
    , configDecoder_v_0_2_0
    , configDecoder_v_0_3_0
    , configDecoder_v_0_4_0
    , configEncoder
    , defaultConfig
    )

import Column exposing (Column)
import Date exposing (Date)
import Filter exposing (Filter, Polarity(..))
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Config =
    { completedCount : Int
    , filters : List Filter
    , filterPolarity : Polarity
    , showFilteredTags : Bool
    , includeUndated : Bool
    , title : String
    }


defaultConfig : Config
defaultConfig =
    { completedCount = 10
    , filters = []
    , filterPolarity = Filter.defaultPolarity
    , showFilteredTags = True
    , includeUndated = True
    , title = ""
    }



-- SERIALIZATION


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "filters" .filters <| TsEncode.list Filter.encoder
        , TsEncode.required "filterPolarity" .filterPolarity Filter.polarityEncoder
        , TsEncode.required "showFilteredTags" .showFilteredTags TsEncode.bool
        , TsEncode.required "includeUndated" .includeUndated TsEncode.bool
        , TsEncode.required "title" .title TsEncode.string
        ]


configDecoder_v_0_4_0 : TsDecode.Decoder Config
configDecoder_v_0_4_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.field "showFilteredTags" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_3_0 : TsDecode.Decoder Config
configDecoder_v_0_3_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.field "filterPolarity" <| Filter.polarityDecoder)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_2_0 : TsDecode.Decoder Config
configDecoder_v_0_2_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filters" <| TsDecode.list Filter.decoder)
        |> TsDecode.andMap (TsDecode.succeed Allow)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_1_0 : TsDecode.Decoder Config
configDecoder_v_0_1_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed [])
        |> TsDecode.andMap (TsDecode.succeed Allow)
        |> TsDecode.andMap (TsDecode.succeed False)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)



-- COLUMNS


taskListWithTagsRemoved : Config -> TaskList -> TaskList
taskListWithTagsRemoved config taskList =
    let
        filterTags : List String
        filterTags =
            config.filters
                |> List.filter (\f -> Filter.filterType f == "Tags")
                |> List.map Filter.value

        tagsToRemove : List String
        tagsToRemove =
            if config.showFilteredTags then
                []

            else
                filterTags
    in
    TaskList.removeTags tagsToRemove taskList


columns : TimeWithZone -> Config -> TaskList -> List (Column TaskItem)
columns timeWithZone config taskList =
    let
        tasks : TaskList
        tasks =
            taskListWithTagsRemoved config taskList

        datestamp : Date
        datestamp =
            TimeWithZone.toDate timeWithZone
    in
    [ Column.init "Today" <| todaysItems datestamp tasks
    , Column.init "Tomorrow" <| tomorrowsItems datestamp tasks
    , Column.init "Future" <| futureItems datestamp tasks
    ]
        |> prependUndated tasks config
        |> appendCompleted tasks config


prependUndated : TaskList -> Config -> List (Column TaskItem) -> List (Column TaskItem)
prependUndated taskList config columnList =
    let
        undatedtasks : List TaskItem
        undatedtasks =
            TaskList.topLevelTasks taskList
                |> List.filter (\t -> (not <| TaskItem.isCompleted t) && (not <| TaskItem.isDated t))
                |> List.sortBy (String.toLower << TaskItem.title)
    in
    if config.includeUndated then
        Column.init "Undated" undatedtasks :: columnList

    else
        columnList


todaysItems : Date -> TaskList -> List TaskItem
todaysItems today taskList =
    let
        isToday : TaskItem -> Bool
        isToday t =
            case TaskItem.due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days today date <= 0 then
                        True

                    else
                        False
    in
    TaskList.topLevelTasks taskList
        |> List.filter (\t -> (not <| TaskItem.isCompleted t) && isToday t)
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


tomorrowsItems : Date -> TaskList -> List TaskItem
tomorrowsItems today taskList =
    let
        tomorrow : Date
        tomorrow =
            Date.add Date.Days 1 today

        isTomorrow : TaskItem -> Bool
        isTomorrow t =
            case TaskItem.due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days tomorrow date == 0 then
                        True

                    else
                        False
    in
    TaskList.topLevelTasks taskList
        |> List.filter (\t -> isTomorrow t && (not <| TaskItem.isCompleted t))
        |> List.sortBy (String.toLower << TaskItem.title)


futureItems : Date -> TaskList -> List TaskItem
futureItems today taskList =
    let
        tomorrow : Date
        tomorrow =
            Date.add Date.Days 1 today

        isToday : TaskItem -> Bool
        isToday t =
            case TaskItem.due t of
                Nothing ->
                    False

                Just date ->
                    if Date.diff Date.Days tomorrow date > 0 then
                        True

                    else
                        False
    in
    TaskList.topLevelTasks taskList
        |> List.filter (\t -> (not <| TaskItem.isCompleted t) && isToday t)
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie


appendCompleted : TaskList -> Config -> List (Column TaskItem) -> List (Column TaskItem)
appendCompleted taskList config columnList =
    if config.completedCount > 0 then
        TaskList.topLevelTasks taskList
            |> List.filter TaskItem.isCompleted
            |> List.sortBy (String.toLower << TaskItem.title)
            |> List.reverse
            |> List.sortBy TaskItem.completedPosix
            |> List.reverse
            |> List.take config.completedCount
            |> Column.init "Completed"
            |> List.singleton
            |> List.append columnList

    else
        columnList
