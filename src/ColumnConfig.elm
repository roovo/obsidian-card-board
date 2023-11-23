module ColumnConfig exposing
    ( ColumnConfig
    , completed
    , defaultUndated
    , futureColumn
    , todayColumn
    , tomorrowColumn
    )

import ColumnConfig.Completed as CompletedColumnConfig exposing (CompletedConfig)
import ColumnConfig.Date as DateColumnConfig exposing (DateConfig)
import ColumnConfig.Undated as UndatedColumnConfig exposing (UndatedConfig)



-- TYPES


type ColumnConfig
    = Completed CompletedConfig
    | Date DateConfig
    | Undated UndatedConfig



-- CONSTRUCTION


completed : Int -> ColumnConfig
completed limit =
    Completed <| CompletedColumnConfig.init limit


defaultUndated : ColumnConfig
defaultUndated =
    Undated UndatedColumnConfig.init


futureColumn : ColumnConfig
futureColumn =
    Date DateColumnConfig.future


todayColumn : ColumnConfig
todayColumn =
    Date DateColumnConfig.today


tomorrowColumn : ColumnConfig
tomorrowColumn =
    Date DateColumnConfig.tomorrow
