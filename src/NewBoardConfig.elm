module NewBoardConfig exposing
    ( NewBoardConfig
    , OptionsForSelect
    , default
    , optionsForSelect
    , updateBoardType
    , updateName
    )

-- TYPES


type alias NewBoardConfig =
    { name : String
    , boardType : String
    }


type alias OptionsForSelect =
    { isSelected : Bool
    , text : String
    , value : String
    }



-- INITIALIZE


default : NewBoardConfig
default =
    { name = ""
    , boardType = "emptyBoard"
    }



-- INFO


optionsForSelect : NewBoardConfig -> List OptionsForSelect
optionsForSelect newBoardConfig =
    [ { isSelected = newBoardConfig.boardType == "dateBoard"
      , text = "Date Board"
      , value = "dateBoard"
      }
    , { isSelected =
            (newBoardConfig.boardType /= "dateBoard")
                && (newBoardConfig.boardType /= "tagBoard")
      , text = "Empty Board"
      , value = "emptyBoard"
      }
    , { isSelected = newBoardConfig.boardType == "tagBoard"
      , text = "Tag Board"
      , value = "tagBoard"
      }
    ]



-- MODIFICATION


updateBoardType : String -> NewBoardConfig -> NewBoardConfig
updateBoardType newType newBoardConfig =
    { newBoardConfig | boardType = newType }


updateName : String -> NewBoardConfig -> NewBoardConfig
updateName newName newBoardConfig =
    { newBoardConfig | name = newName }
