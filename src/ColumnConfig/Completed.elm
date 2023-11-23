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


init : CompletedConfig
init =
    CompletedConfig { name = "Completed", limit = 10 }
