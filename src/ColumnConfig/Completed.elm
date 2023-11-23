module ColumnConfig.Completed exposing
    ( CompletedConfig
    , init
    )

-- TYPES


type CompletedConfig
    = CompletedConfig Config


type alias Config =
    { name : String
    , limit : Int
    }



-- CONSTRUCTION


init : Int -> CompletedConfig
init limit =
    CompletedConfig { name = "Completed", limit = limit }
