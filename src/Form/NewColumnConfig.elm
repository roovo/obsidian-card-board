module Form.NewColumnConfig exposing
    ( NewColumnConfigForm
    , default
    , updateColumnType
    , updateName
    )

-- TYPES


type alias NewColumnConfigForm =
    { name : String
    , columnType : String
    }



-- INITIALIZE


default : NewColumnConfigForm
default =
    { name = ""
    , columnType = ""
    }



-- MODIFICATION


updateColumnType : String -> NewColumnConfigForm -> NewColumnConfigForm
updateColumnType newType newColumnConfigForm =
    { newColumnConfigForm | columnType = newType }


updateName : String -> NewColumnConfigForm -> NewColumnConfigForm
updateName newName newColumnConfigForm =
    { newColumnConfigForm | name = newName }
