module DateBoard exposing
    ( Config
    , DateBoard
    , columns
    , fill
    )

import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type DateBoard
    = DateBoard Config TaskList


type alias Config =
    { includeUndated : Bool
    }


fill : Config -> TaskList -> DateBoard
fill config taskList =
    DateBoard config taskList



-- COLUMNS


columns : Time.Posix -> Time.Zone -> DateBoard -> List ( String, List TaskItem )
columns now zone dateBoard =
    [ ( "Today"
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
        |> prependUndated dateBoard


prependUndated : DateBoard -> List ( String, List TaskItem ) -> List ( String, List TaskItem )
prependUndated (DateBoard config taskList) columnList =
    let
        undatedtasks =
            TaskList.topLevelTasks taskList
                |> List.filter (\t -> (not <| TaskItem.isCompleted t) && (not <| TaskItem.isDated t))
                |> List.sortBy TaskItem.filePath
    in
    if config.includeUndated then
        ( "Undated", undatedtasks ) :: columnList

    else
        columnList


todaysItems : Date -> DateBoard -> List TaskItem
todaysItems today (DateBoard config taskList) =
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
tomorrowsItems today (DateBoard config taskList) =
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
futureItems today (DateBoard config taskList) =
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
completedItems (DateBoard config taskList) =
    TaskList.topLevelTasks taskList
        |> List.filter TaskItem.isCompleted
        |> List.sortBy TaskItem.filePath
        |> List.sortBy TaskItem.completedPosix
        |> List.reverse
