module DateBoard exposing
    ( Config
    , columns
    , configDecoder
    , configDecoder_v_0_1_0
    , configEncoder
    , defaultConfig
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Config =
    { completedCount : Int
    , filterPaths : String
    , filterTags : String
    , includeUndated : Bool
    , title : String
    }


defaultConfig : Config
defaultConfig =
    { completedCount = 10
    , filterPaths = ""
    , filterTags = ""
    , includeUndated = True
    , title = ""
    }



-- SERIALIZATION


configEncoder : TsEncode.Encoder Config
configEncoder =
    TsEncode.object
        [ TsEncode.required "completedCount" .completedCount TsEncode.int
        , TsEncode.required "includeUndated" .includeUndated TsEncode.bool
        , TsEncode.required "filterPaths" .filterPaths TsEncode.string
        , TsEncode.required "filterTags" .filterTags TsEncode.string
        , TsEncode.required "title" .title TsEncode.string
        ]


configDecoder : TsDecode.Decoder Config
configDecoder =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "filterPaths" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "filterTags" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)


configDecoder_v_0_1_0 : TsDecode.Decoder Config
configDecoder_v_0_1_0 =
    TsDecode.succeed Config
        |> TsDecode.andMap (TsDecode.field "completedCount" TsDecode.int)
        |> TsDecode.andMap (TsDecode.succeed "")
        |> TsDecode.andMap (TsDecode.succeed "")
        |> TsDecode.andMap (TsDecode.field "includeUndated" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "title" TsDecode.string)



-- COLUMNS


columns : TimeWithZone -> Config -> TaskList -> List ( String, List TaskItem )
columns timeWithZone config taskList =
    let
        datestamp =
            TimeWithZone.toDate timeWithZone
    in
    [ ( "Today"
      , todaysItems datestamp taskList
      )
    , ( "Tomorrow"
      , tomorrowsItems datestamp taskList
      )
    , ( "Future"
      , futureItems datestamp taskList
      )
    ]
        |> prependUndated taskList config
        |> appendCompleted taskList config


prependUndated : TaskList -> Config -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependUndated taskList config columnList =
    let
        undatedtasks =
            TaskList.topLevelTasks taskList
                |> List.filter (\t -> (not <| TaskItem.isCompleted t) && (not <| TaskItem.isDated t))
                |> List.sortBy (String.toLower << TaskItem.title)
    in
    if config.includeUndated then
        ( "Undated", undatedtasks ) :: columnList

    else
        columnList


todaysItems : Date -> TaskList -> List TaskItem
todaysItems today taskList =
    let
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
        tomorrow =
            Date.add Date.Days 1 today

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
        tomorrow =
            Date.add Date.Days 1 today

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


appendCompleted : TaskList -> Config -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
appendCompleted taskList config columnList =
    if config.completedCount > 0 then
        TaskList.topLevelTasks taskList
            |> List.filter TaskItem.isCompleted
            |> List.sortBy (String.toLower << TaskItem.title)
            |> List.reverse
            |> List.sortBy TaskItem.completedPosix
            |> List.reverse
            |> List.take config.completedCount
            |> Tuple.pair "Completed"
            |> List.singleton
            |> List.append columnList

    else
        columnList
