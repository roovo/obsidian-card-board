module Column.Date exposing
    ( DateColumn
    , addTaskItem
    , asColumn
    , init
    )

import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import DateBoardConfig exposing (DateBoardConfig)
import Filter
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type DateColumn
    = DateColumn Config


type alias Config =
    { name : String
    , from : Maybe Int
    , to : Maybe Int
    , taskList : TaskList
    , tagsToHide : List String
    }



-- CONSTRUCTION


init : DateBoardConfig -> { name : String, from : Maybe Int, to : Maybe Int } -> DateColumn
init dateBoardConfig c =
    let
        filterTagsToHide : List String
        filterTagsToHide =
            if dateBoardConfig.showFilteredTags then
                []

            else
                dateBoardConfig
                    |> .filters
                    |> List.filter (\f -> Filter.filterType f == "Tags")
                    |> List.map Filter.value
    in
    DateColumn
        { name = c.name
        , from = c.from
        , to = c.to
        , taskList = TaskList.empty
        , tagsToHide = filterTagsToHide
        }


addTaskItem : TimeWithZone -> TaskItem -> DateColumn -> ( DateColumn, Column.PlacementResult )
addTaskItem now taskItem ((DateColumn c) as dateColumn) =
    let
        today =
            TimeWithZone.toDate now

        from =
            Maybe.map (\offset -> Date.add Date.Days offset today) c.from

        to =
            Maybe.map (\offset -> Date.add Date.Days offset today) c.to
    in
    case TaskItem.due taskItem of
        Just dueDate ->
            if belongs from to dueDate then
                if isCompleted taskItem then
                    ( dateColumn, Column.CompletedInThisColumn )

                else
                    ( DateColumn { c | taskList = TaskList.add taskItem c.taskList }, Column.Placed )

            else
                ( dateColumn, Column.DoesNotBelong )

        Nothing ->
            ( dateColumn, Column.DoesNotBelong )



-- INFO


asColumn : DateColumn -> Column TaskItem
asColumn dateColumn =
    config dateColumn
        |> .taskList
        |> TaskList.removeTags (tagsToRemove dateColumn)
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init True (name dateColumn)



-- PRIVATE


belongs : Maybe Date -> Maybe Date -> Date -> Bool
belongs from to taskDate =
    case ( from, to ) of
        ( Just f, Just t ) ->
            Date.isBetween f t taskDate

        ( Nothing, Just t ) ->
            Date.diff Date.Days taskDate t >= 0

        ( Just f, Nothing ) ->
            Date.diff Date.Days f taskDate >= 0

        _ ->
            True


config : DateColumn -> Config
config (DateColumn c) =
    c


isCompleted : TaskItem -> Bool
isCompleted =
    TaskItem.isCompleted


name : DateColumn -> String
name =
    .name << config


tagsToRemove : DateColumn -> List String
tagsToRemove =
    .tagsToHide << config
