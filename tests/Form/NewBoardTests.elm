module Form.NewBoardTests exposing (suite)

import Expect
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Test exposing (..)


suite : Test
suite =
    concat
        [ default
        , optionsForSelect
        , updateBoardType
        , updateName
        ]


default : Test
default =
    describe "default"
        [ test "returns config with no name" <|
            \() ->
                NewBoardForm.default
                    |> .name
                    |> Expect.equal ""
        , test "returns config with the emptyBoard boardType" <|
            \() ->
                NewBoardForm.default
                    |> .boardType
                    |> Expect.equal "emptyBoard"
        ]


optionsForSelect : Test
optionsForSelect =
    describe "optionsForSelect"
        [ test "can return the information with the emptyBoard boardType selected" <|
            \() ->
                NewBoardForm.optionsForSelect (NewBoardForm "" "emptyBoard")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Date Board"
                          , value = "dateBoard"
                          }
                        , { isSelected = True
                          , text = "Empty Board"
                          , value = "emptyBoard"
                          }
                        , { isSelected = False
                          , text = "Tag Board"
                          , value = "tagBoard"
                          }
                        ]
        , test "returns the information with the emptyBoard boardType selected for an unrecognised board type" <|
            \() ->
                NewBoardForm.optionsForSelect (NewBoardForm "" "xxxBoard")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Date Board"
                          , value = "dateBoard"
                          }
                        , { isSelected = True
                          , text = "Empty Board"
                          , value = "emptyBoard"
                          }
                        , { isSelected = False
                          , text = "Tag Board"
                          , value = "tagBoard"
                          }
                        ]
        , test "can return the information with the dateBoard boardType selected" <|
            \() ->
                NewBoardForm.optionsForSelect (NewBoardForm "" "dateBoard")
                    |> Expect.equal
                        [ { isSelected = True
                          , text = "Date Board"
                          , value = "dateBoard"
                          }
                        , { isSelected = False
                          , text = "Empty Board"
                          , value = "emptyBoard"
                          }
                        , { isSelected = False
                          , text = "Tag Board"
                          , value = "tagBoard"
                          }
                        ]
        , test "can return the information with the tagBoard boardType selected" <|
            \() ->
                NewBoardForm.optionsForSelect (NewBoardForm "" "tagBoard")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Date Board"
                          , value = "dateBoard"
                          }
                        , { isSelected = False
                          , text = "Empty Board"
                          , value = "emptyBoard"
                          }
                        , { isSelected = True
                          , text = "Tag Board"
                          , value = "tagBoard"
                          }
                        ]
        ]


updateBoardType : Test
updateBoardType =
    describe "updateBoardType"
        [ test "updates the board name" <|
            \() ->
                NewBoardForm.default
                    |> NewBoardForm.updateBoardType "foo"
                    |> .boardType
                    |> Expect.equal "foo"
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the board name" <|
            \() ->
                NewBoardForm.default
                    |> NewBoardForm.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
        ]
