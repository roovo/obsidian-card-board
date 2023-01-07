module Column.Undated exposing
    ( UndatedColumn
    , addTaskItem
    , asColumn
    , init
    )

import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import DateBoard
import Filter
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)



-- TYPES


type UndatedColumn
    = UndatedColumn Config


type alias Config =
    { enabled : Bool
    , name : String
    , taskList : TaskList
    , tagsToHide : List String
    }



-- CONSTRUCTION


init : DateBoard.Config -> ColumnNames -> UndatedColumn
init dateBoardConfig columnNames =
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
    UndatedColumn
        { enabled = dateBoardConfig.includeUndated
        , name = ColumnNames.nameFor "undated" columnNames
        , taskList = TaskList.empty
        , tagsToHide = filterTagsToHide
        }


addTaskItem : TaskItem -> UndatedColumn -> ( UndatedColumn, Column.PlacementResult )
addTaskItem taskItem ((UndatedColumn c) as undatedColumn) =
    if isEnabled undatedColumn && belongs taskItem then
        if isCompleted taskItem then
            ( undatedColumn, Column.CompletedInThisColumn )

        else
            ( UndatedColumn { c | taskList = TaskList.add taskItem c.taskList }, Column.Placed )

    else
        ( undatedColumn, Column.DoesNotBelong )



-- INFO


asColumn : UndatedColumn -> Column TaskItem
asColumn undatedColumn =
    config undatedColumn
        |> .taskList
        |> TaskList.removeTags (tagsToRemove undatedColumn)
        |> TaskList.topLevelTasks
        |> List.sortBy (String.toLower << TaskItem.title)
        |> Column.init (isEnabled undatedColumn) (name undatedColumn)



-- PRIVATE


belongs : TaskItem -> Bool
belongs =
    not << TaskItem.isDated


config : UndatedColumn -> Config
config (UndatedColumn c) =
    c


isCompleted : TaskItem -> Bool
isCompleted taskItem =
    TaskItem.isCompleted taskItem


isEnabled : UndatedColumn -> Bool
isEnabled =
    .enabled << config


name : UndatedColumn -> String
name =
    .name << config


tagsToRemove : UndatedColumn -> List String
tagsToRemove =
    .tagsToHide << config
