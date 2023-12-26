module Form.NewBoardConfig exposing
    ( NewBoardConfigForm
    , OptionsForSelect
    , default
    , optionsForSelect
    , updateBoardType
    , updateName
    )

-- TYPES


type alias NewBoardConfigForm =
    { name : String
    , boardType : String
    }


type alias OptionsForSelect =
    { isSelected : Bool
    , text : String
    , value : String
    }



-- INITIALIZE


default : NewBoardConfigForm
default =
    { name = ""
    , boardType = "emptyBoard"
    }



-- INFO


optionsForSelect : NewBoardConfigForm -> List OptionsForSelect
optionsForSelect newBoardConfigForm =
    [ { isSelected = newBoardConfigForm.boardType == "dateBoard"
      , text = "Date Board"
      , value = "dateBoard"
      }
    , { isSelected =
            (newBoardConfigForm.boardType /= "dateBoard")
                && (newBoardConfigForm.boardType /= "tagBoard")
      , text = "Empty Board"
      , value = "emptyBoard"
      }
    , { isSelected = newBoardConfigForm.boardType == "tagBoard"
      , text = "Tag Board"
      , value = "tagBoard"
      }
    ]



-- MODIFICATION


updateBoardType : String -> NewBoardConfigForm -> NewBoardConfigForm
updateBoardType newType newBoardConfigForm =
    { newBoardConfigForm | boardType = newType }


updateName : String -> NewBoardConfigForm -> NewBoardConfigForm
updateName newName newBoardConfigForm =
    { newBoardConfigForm | name = newName }
