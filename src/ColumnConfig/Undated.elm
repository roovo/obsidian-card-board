module ColumnConfig.Undated exposing
    ( UndatedConfig
    , init
    )

-- TYPES


type UndatedConfig
    = UndatedConfig Config


type alias Config =
    { name : String
    }



-- CONSTRUCTION


init : UndatedConfig
init =
    UndatedConfig { name = "Undated" }
