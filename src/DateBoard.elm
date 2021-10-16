module DateBoard exposing
    ( Config
    , columns
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type alias Config =
    { includeCompleted : Bool
    , includeUndated : Bool
    , title : String
    }



-- COLUMNS


columns : Time.Posix -> Time.Zone -> Config -> Int -> TaskList -> List ( String, List TaskItem )
columns now zone config panelIndex taskList =
    [ ( "Today"
      , todaysItems (Date.fromPosix zone now) panelIndex taskList config
      )
    , ( "Tomorrow"
      , tomorrowsItems (Date.fromPosix zone now) panelIndex taskList config
      )
    , ( "Future"
      , futureItems (Date.fromPosix zone now) panelIndex taskList config
      )
    ]
        |> prependUndated panelIndex taskList config
        |> appendCompleted panelIndex taskList config


prependUndated : Int -> TaskList -> Config -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependUndated panelIndex taskList config columnList =
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


todaysItems : Date -> Int -> TaskList -> Config -> List TaskItem
todaysItems today panelIndex taskList config =
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


tomorrowsItems : Date -> Int -> TaskList -> Config -> List TaskItem
tomorrowsItems today panelIndex taskList config =
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


futureItems : Date -> Int -> TaskList -> Config -> List TaskItem
futureItems today panelIndex taskList config =
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


appendCompleted : Int -> TaskList -> Config -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
appendCompleted panelIndex taskList config columnList =
    let
        completedTasks =
            TaskList.topLevelTasks taskList
                |> List.filter TaskItem.isCompleted
                |> List.sortBy (String.toLower << TaskItem.title)
                |> List.reverse
                |> List.sortBy TaskItem.completedPosix
                |> List.reverse
    in
    if config.includeCompleted then
        List.append columnList [ ( "Completed", completedTasks ) ]

    else
        columnList
