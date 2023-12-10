module NewColumnConfig exposing
    ( NewColumnConfig
    , OptionsForSelect
    , default
      -- , optionsForSelect
      -- , updateBoardType
      -- , updateName
    )

-- TYPES


type alias NewColumnConfig =
    { name : String
    , columnType : String
    }


type alias OptionsForSelect =
    { isSelected : Bool
    , text : String
    , value : String
    }



-- INITIALIZE


default : NewColumnConfig
default =
    { name = ""
    , columnType = ""
    }



-- INFO


optionsForSelect : NewColumnConfig -> List OptionsForSelect
optionsForSelect newColumnConfig =
    [ { isSelected = newColumnConfig.columnType == "dateBoard"
      , text = "Date Board"
      , value = "dateBoard"
      }
    , { isSelected =
            (newColumnConfig.columnType /= "dateBoard")
                && (newColumnConfig.columnType /= "tagBoard")
      , text = "Empty Board"
      , value = "emptyBoard"
      }
    , { isSelected = newColumnConfig.columnType == "tagBoard"
      , text = "Tag Board"
      , value = "tagBoard"
      }
    ]



-- MODIFICATION


updateBoardType : String -> NewColumnConfig -> NewColumnConfig
updateBoardType newType newColumnConfig =
    { newColumnConfig | columnType = newType }


updateName : String -> NewColumnConfig -> NewColumnConfig
updateName newName newColumnConfig =
    { newColumnConfig | name = newName }
