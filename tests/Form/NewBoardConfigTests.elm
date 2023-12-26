module Form.NewBoardConfigTests exposing (suite)

import Expect
import Form.NewBoardConfig as NewBoardConfigForm exposing (NewBoardConfigForm)
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
                NewBoardConfigForm.default
                    |> .name
                    |> Expect.equal ""
        , test "returns config with the emptyBoard boardType" <|
            \() ->
                NewBoardConfigForm.default
                    |> .boardType
                    |> Expect.equal "emptyBoard"
        ]


optionsForSelect : Test
optionsForSelect =
    describe "optionsForSelect"
        [ test "can return the information with the emptyBoard boardType selected" <|
            \() ->
                NewBoardConfigForm.optionsForSelect (NewBoardConfigForm "" "emptyBoard")
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
                NewBoardConfigForm.optionsForSelect (NewBoardConfigForm "" "xxxBoard")
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
                NewBoardConfigForm.optionsForSelect (NewBoardConfigForm "" "dateBoard")
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
                NewBoardConfigForm.optionsForSelect (NewBoardConfigForm "" "tagBoard")
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
                NewBoardConfigForm.default
                    |> NewBoardConfigForm.updateBoardType "foo"
                    |> .boardType
                    |> Expect.equal "foo"
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the board name" <|
            \() ->
                NewBoardConfigForm.default
                    |> NewBoardConfigForm.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
        ]
