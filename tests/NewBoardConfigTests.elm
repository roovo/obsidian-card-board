module NewBoardConfigTests exposing (suite)

import Expect
import NewBoardConfig exposing (NewBoardConfig)
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
                NewBoardConfig.default
                    |> .name
                    |> Expect.equal ""
        , test "returns config with the emptyBoard boardType" <|
            \() ->
                NewBoardConfig.default
                    |> .boardType
                    |> Expect.equal "emptyBoard"
        ]


optionsForSelect : Test
optionsForSelect =
    describe "optionsForSelect"
        [ test "can return the information with the emptyBoard boardType selected" <|
            \() ->
                NewBoardConfig.optionsForSelect (NewBoardConfig "" "emptyBoard")
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
                NewBoardConfig.optionsForSelect (NewBoardConfig "" "xxxBoard")
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
                NewBoardConfig.optionsForSelect (NewBoardConfig "" "dateBoard")
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
                NewBoardConfig.optionsForSelect (NewBoardConfig "" "tagBoard")
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
                NewBoardConfig.default
                    |> NewBoardConfig.updateBoardType "foo"
                    |> .boardType
                    |> Expect.equal "foo"
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the board name" <|
            \() ->
                NewBoardConfig.default
                    |> NewBoardConfig.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
        ]
