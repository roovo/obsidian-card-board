module NewColumnConfig exposing
    ( NewColumnConfig
    , default
    , updateColumnType
    , updateName
    )

import List.Extra as LE



-- TYPES


type alias NewColumnConfig =
    { name : String
    , columnType : String
    }



-- INITIALIZE


default : NewColumnConfig
default =
    { name = ""
    , columnType = ""
    }



-- MODIFICATION


updateColumnType : String -> NewColumnConfig -> NewColumnConfig
updateColumnType newType newColumnConfig =
    { newColumnConfig | columnType = newType }


updateName : String -> NewColumnConfig -> NewColumnConfig
updateName newName newColumnConfig =
    { newColumnConfig | name = newName }
