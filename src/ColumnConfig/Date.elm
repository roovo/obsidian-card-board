module ColumnConfig.Date exposing
    ( DateConfig
    , future
    , today
    , tomorrow
    )

-- TYPES


type DateConfig
    = DateConfig Config


type alias Config =
    { name : String
    , range : RelativeDateRange
    }


type RelativeDateRange
    = Between Int Int
    | Before Int
    | After Int



-- CONSTRUCTION


today : DateConfig
today =
    DateConfig { name = "Today", range = Before 1 }


tomorrow : DateConfig
tomorrow =
    DateConfig { name = "Tomorrow", range = Between 1 1 }


future : DateConfig
future =
    DateConfig { name = "Future", range = After 1 }
