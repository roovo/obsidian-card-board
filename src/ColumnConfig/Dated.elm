module ColumnConfig.Dated exposing
    ( DatedColumn
    , RelativeDateRange(..)
    , addTaskItem
    , asColumn
    , forToday
    , future
    , init
    , name
    , tomorrow
    )

import Column exposing (Column, PlacementResult)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type DatedColumn
    = DatedColumn Config TaskList


type alias Config =
    { collapsed : Bool
    , name : String
    , range : RelativeDateRange
    }


type RelativeDateRange
    = Between Int Int
    | Before Int
    | After Int



-- CONSTRUCTION


init : String -> RelativeDateRange -> DatedColumn
init name_ range_ =
    DatedColumn { collapsed = False, name = name_, range = range_ } TaskList.empty


forToday : DatedColumn
forToday =
    DatedColumn { collapsed = False, name = "Today", range = Before 1 } TaskList.empty


tomorrow : DatedColumn
tomorrow =
    DatedColumn { collapsed = False, name = "Tomorrow", range = Between 1 1 } TaskList.empty


future : DatedColumn
future =
    DatedColumn { collapsed = False, name = "Future", range = After 1 } TaskList.empty



-- INFO


asColumn : DatedColumn -> Column TaskItem
asColumn ((DatedColumn c tl) as datedColumn) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie
        |> Column.init True (name datedColumn) []
        |> Column.collapseState c.collapsed


name : DatedColumn -> String
name (DatedColumn c _) =
    c.name


taskList : DatedColumn -> TaskList
taskList (DatedColumn _ tl) =
    tl



-- MODIFICATION


addTaskItem : Date -> TaskItem -> DatedColumn -> ( DatedColumn, Column.PlacementResult )
addTaskItem today taskItem ((DatedColumn c tl) as datedColumn) =
    case TaskItem.due taskItem of
        Just dueDate ->
            if belongs today c.range dueDate then
                if TaskItem.isCompleted taskItem then
                    ( datedColumn, Column.CompletedInThisColumn )

                else
                    ( DatedColumn c (TaskList.add taskItem tl), Column.Placed )

            else
                ( datedColumn, Column.DoesNotBelong )

        Nothing ->
            ( datedColumn, Column.DoesNotBelong )



-- PRIVATE


belongs : Date -> RelativeDateRange -> Date -> Bool
belongs today range taskDate =
    case range of
        Between from to ->
            Date.isBetween (Date.add Date.Days from today) (Date.add Date.Days to today) taskDate

        Before to ->
            Date.diff Date.Days taskDate (Date.add Date.Days (to - 1) today) >= 0

        After from ->
            Date.diff Date.Days (Date.add Date.Days (from + 1) today) taskDate >= 0
