module Column.Dated exposing
    ( DatedColumn
    , RelativeDateRange(..)
    , addTaskItem
    , forToday
    , future
    , init
    , isCollapsed
    , name
    , setTagsToHide
    , toList
    , tomorrow
    )

import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import PlacementResult exposing (PlacementResult)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type DatedColumn
    = DatedColumn Config (List String) TaskList


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
    DatedColumn { collapsed = False, name = name_, range = range_ } [] TaskList.empty


forToday : DatedColumn
forToday =
    DatedColumn { collapsed = False, name = "Today", range = Before 1 } [] TaskList.empty


tomorrow : DatedColumn
tomorrow =
    DatedColumn { collapsed = False, name = "Tomorrow", range = Between 1 1 } [] TaskList.empty


future : DatedColumn
future =
    DatedColumn { collapsed = False, name = "Future", range = After 1 } [] TaskList.empty



-- INFO


isCollapsed : DatedColumn -> Bool
isCollapsed (DatedColumn c _ _) =
    c.collapsed


name : DatedColumn -> String
name (DatedColumn c _ _) =
    c.name


toList : DatedColumn -> List TaskItem
toList (DatedColumn _ _ tl) =
    tl
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> List.sortBy TaskItem.dueRataDie



-- MODIFICATION


addTaskItem : Date -> TaskItem -> DatedColumn -> ( DatedColumn, PlacementResult )
addTaskItem today taskItem ((DatedColumn c tth tl) as datedColumn) =
    case TaskItem.due taskItem of
        Just dueDate ->
            if belongs today c.range dueDate then
                if TaskItem.isCompleted taskItem then
                    ( datedColumn, PlacementResult.CompletedInThisColumn )

                else
                    ( DatedColumn c tth (TaskList.add taskItem tl), PlacementResult.Placed )

            else
                ( datedColumn, PlacementResult.DoesNotBelong )

        Nothing ->
            ( datedColumn, PlacementResult.DoesNotBelong )


setTagsToHide : List String -> DatedColumn -> DatedColumn
setTagsToHide tags (DatedColumn c _ tl) =
    DatedColumn c tags tl



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
