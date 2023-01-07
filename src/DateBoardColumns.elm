module DateBoardColumns exposing
    ( DateBoardColumns
    , addTaskItem
    , columns
    , init
    )

import Column exposing (Column, PlacementResult)
import Column.Completed as CompletedColumn exposing (CompletedColumn)
import Column.Date as DateColumn exposing (DateColumn)
import Column.Undated as UndatedColumn exposing (UndatedColumn)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import DateBoardConfig exposing (DateBoardConfig)
import Filter exposing (Filter, Polarity(..))
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TimeWithZone exposing (TimeWithZone)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type DateBoardColumns
    = DateBoardColumns Config


type alias Config =
    { now : TimeWithZone
    , undatedColumn : UndatedColumn
    , dateColumns : List DateColumn
    , completedColumn : CompletedColumn
    }



-- BUILDING


init : TimeWithZone -> ColumnNames -> DateBoardConfig -> DateBoardColumns
init now columnNames dateBoardConfig =
    let
        todayName =
            ColumnNames.nameFor "today" columnNames

        tomorrowName =
            ColumnNames.nameFor "tomorrow" columnNames

        futureName =
            ColumnNames.nameFor "future" columnNames
    in
    DateBoardColumns
        { now = now
        , undatedColumn = UndatedColumn.init dateBoardConfig columnNames
        , dateColumns =
            [ DateColumn.init dateBoardConfig { name = todayName, from = Nothing, to = Just 0 }
            , DateColumn.init dateBoardConfig { name = tomorrowName, from = Just 1, to = Just 1 }
            , DateColumn.init dateBoardConfig { name = futureName, from = Just 2, to = Nothing }
            ]
        , completedColumn = CompletedColumn.forDateBoard dateBoardConfig columnNames
        }


addTaskItem : TaskItem -> DateBoardColumns -> DateBoardColumns
addTaskItem taskItem (DateBoardColumns config) =
    let
        addToUndatedColumn : ( Config, List PlacementResult )
        addToUndatedColumn =
            let
                ( newColumn, pr ) =
                    UndatedColumn.addTaskItem taskItem config.undatedColumn
            in
            ( { config | undatedColumn = newColumn }, [ pr ] )

        addToDateColumn : DateColumn -> ( DateColumn, PlacementResult )
        addToDateColumn dateColumn =
            DateColumn.addTaskItem config.now taskItem dateColumn

        addToDateColumns : ( Config, List PlacementResult ) -> ( Config, List PlacementResult )
        addToDateColumns ( c, prs ) =
            let
                ( newColumns, pr ) =
                    List.foldl bar ( [], [] ) config.dateColumns

                bar : DateColumn -> ( List DateColumn, List PlacementResult ) -> ( List DateColumn, List PlacementResult )
                bar ntc ( ntcs, pr_ ) =
                    addToDateColumn ntc
                        |> Tuple.mapFirst (\r -> r :: ntcs)
                        |> Tuple.mapSecond (\r -> r :: pr_)
            in
            ( { c | dateColumns = List.reverse newColumns }, pr ++ prs )

        addToCompletedColumn : ( Config, List PlacementResult ) -> Config
        addToCompletedColumn ( c, prs ) =
            { c | completedColumn = CompletedColumn.addTaskItem prs taskItem config.completedColumn }
    in
    addToUndatedColumn
        |> addToDateColumns
        |> addToCompletedColumn
        |> DateBoardColumns



-- INFO


columns : DateBoardColumns -> List (Column TaskItem)
columns (DateBoardColumns config) =
    [ CompletedColumn.asColumn config.completedColumn ]
        |> List.append (List.map DateColumn.asColumn config.dateColumns)
        |> List.append [ UndatedColumn.asColumn config.undatedColumn ]
        |> List.filter Column.isEnabled
