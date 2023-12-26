module Form.NewBoard exposing
    ( NewBoardForm
    , OptionsForSelect
    , default
    , optionsForSelect
    , updateBoardType
    , updateName
    )

-- TYPES


type alias NewBoardForm =
    { name : String
    , boardType : String
    }


type alias OptionsForSelect =
    { isSelected : Bool
    , text : String
    , value : String
    }



-- INITIALIZE


default : NewBoardForm
default =
    { name = ""
    , boardType = "emptyBoard"
    }



-- safeDecoder : SD.Decoder NewBoardForm BoardConfigForm.Form
-- safeDecoder =
-- INFO


optionsForSelect : NewBoardForm -> List OptionsForSelect
optionsForSelect newBoardForm =
    [ { isSelected = newBoardForm.boardType == "dateBoard"
      , text = "Date Board"
      , value = "dateBoard"
      }
    , { isSelected =
            (newBoardForm.boardType /= "dateBoard")
                && (newBoardForm.boardType /= "tagBoard")
      , text = "Empty Board"
      , value = "emptyBoard"
      }
    , { isSelected = newBoardForm.boardType == "tagBoard"
      , text = "Tag Board"
      , value = "tagBoard"
      }
    ]



-- MODIFICATION


updateBoardType : String -> NewBoardForm -> NewBoardForm
updateBoardType newType newBoardForm =
    { newBoardForm | boardType = newType }


updateName : String -> NewBoardForm -> NewBoardForm
updateName newName newBoardForm =
    { newBoardForm | name = newName }
