module DateBoard exposing
    ( DateBoard
    , columns
    , fill
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type DateBoard
    = DateBoard TaskList


fill : TaskList -> DateBoard
fill taskList =
    DateBoard taskList



-- COLUMNS


columns : Time.Posix -> Time.Zone -> DateBoard -> List ( String, List TaskItem )
columns now zone dateBoard =
    [ ( "Undated"
      , undatedItems dateBoard
      )
    , ( "Today"
      , todaysItems (Date.fromPosix zone now) dateBoard
      )
    , ( "Tomorrow"
      , tomorrowsItems (Date.fromPosix zone now) dateBoard
      )
    , ( "Future"
      , futureItems (Date.fromPosix zone now) dateBoard
      )
    , ( "Done"
      , completedItems dateBoard
      )
    ]


undatedItems : DateBoard -> List TaskItem
undatedItems (DateBoard taskList) =
    TaskList.topLevelTasks taskList
        |> List.filter (\t -> (not <| TaskItem.isCompleted t) && (not <| TaskItem.isDated t))
        |> List.sortBy TaskItem.filePath


todaysItems : Date -> DateBoard -> List TaskItem
todaysItems today (DateBoard taskList) =
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
        |> List.sortBy TaskItem.filePath


tomorrowsItems : Date -> DateBoard -> List TaskItem
tomorrowsItems today (DateBoard taskList) =
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


futureItems : Date -> DateBoard -> List TaskItem
futureItems today (DateBoard taskList) =
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
        |> List.sortBy TaskItem.dueRataDie


completedItems : DateBoard -> List TaskItem
completedItems (DateBoard taskList) =
    TaskList.topLevelTasks taskList
        |> List.filter TaskItem.isCompleted
        |> List.sortBy TaskItem.filePath
        |> List.sortBy TaskItem.completedPosix
        |> List.reverse
