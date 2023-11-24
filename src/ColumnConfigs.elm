module ColumnConfigs exposing
    ( ColumnConfigs
    , defaultForDateBoard
    , empty
    , fromList
    )

import ColumnConfig exposing (ColumnConfig)
import ColumnConfig.Completed as CompletedColumnConfig exposing (CompletedConfig)
import ColumnConfig.Date as DateColumnConfig exposing (DateConfig)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedConfig)



-- TYPES


type ColumnConfigs
    = ColumnConfigs (List ColumnConfig)



-- TEMP TO REMOVE


fromList =
    ColumnConfigs



-- CONSTRUCTION


defaultForDateBoard : ColumnConfigs
defaultForDateBoard =
    ColumnConfigs
        [ ColumnConfig.defaultUndated
        , ColumnConfig.todayColumn
        , ColumnConfig.tomorrowColumn
        , ColumnConfig.futureColumn
        , ColumnConfig.completed 10
        ]


empty : ColumnConfigs
empty =
    ColumnConfigs []
