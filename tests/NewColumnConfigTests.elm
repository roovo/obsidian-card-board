module NewColumnConfigTests exposing (suite)

import Expect
import NewColumnConfig exposing (NewColumnConfig)
import Test exposing (..)


suite : Test
suite =
    concat
        [ default

        -- , optionsForSelect
        -- , updateBoardType
        , updateName
        ]


default : Test
default =
    describe "default"
        [ test "returns config with no name" <|
            \() ->
                NewColumnConfig.default
                    |> .name
                    |> Expect.equal ""
        , test "returns config with an empty columnType" <|
            \() ->
                NewColumnConfig.default
                    |> .columnType
                    |> Expect.equal ""
        ]



-- optionsForSelect : Test
-- optionsForSelect =
--     describe "optionsForSelect"
--         [ test "can return the information with the emptyBoard columnType selected" <|
--             \() ->
--                 NewColumnConfig.optionsForSelect (NewColumnConfig "" "emptyBoard")
--                     |> Expect.equal
--                         [ { isSelected = False
--                           , text = "Date Board"
--                           , value = "dateBoard"
--                           }
--                         , { isSelected = True
--                           , text = "Empty Board"
--                           , value = "emptyBoard"
--                           }
--                         , { isSelected = False
--                           , text = "Tag Board"
--                           , value = "tagBoard"
--                           }
--                         ]
--         , test "returns the information with the emptyBoard columnType selected for an unrecognised board type" <|
--             \() ->
--                 NewColumnConfig.optionsForSelect (NewColumnConfig "" "xxxBoard")
--                     |> Expect.equal
--                         [ { isSelected = False
--                           , text = "Date Board"
--                           , value = "dateBoard"
--                           }
--                         , { isSelected = True
--                           , text = "Empty Board"
--                           , value = "emptyBoard"
--                           }
--                         , { isSelected = False
--                           , text = "Tag Board"
--                           , value = "tagBoard"
--                           }
--                         ]
--         , test "can return the information with the dateBoard columnType selected" <|
--             \() ->
--                 NewColumnConfig.optionsForSelect (NewColumnConfig "" "dateBoard")
--                     |> Expect.equal
--                         [ { isSelected = True
--                           , text = "Date Board"
--                           , value = "dateBoard"
--                           }
--                         , { isSelected = False
--                           , text = "Empty Board"
--                           , value = "emptyBoard"
--                           }
--                         , { isSelected = False
--                           , text = "Tag Board"
--                           , value = "tagBoard"
--                           }
--                         ]
--         , test "can return the information with the tagBoard columnType selected" <|
--             \() ->
--                 NewColumnConfig.optionsForSelect (NewColumnConfig "" "tagBoard")
--                     |> Expect.equal
--                         [ { isSelected = False
--                           , text = "Date Board"
--                           , value = "dateBoard"
--                           }
--                         , { isSelected = False
--                           , text = "Empty Board"
--                           , value = "emptyBoard"
--                           }
--                         , { isSelected = True
--                           , text = "Tag Board"
--                           , value = "tagBoard"
--                           }
--                         ]
--         ]
--
--
-- updateBoardType : Test
-- updateBoardType =
--     describe "updateBoardType"
--         [ test "updates the board name" <|
--             \() ->
--                 NewColumnConfig.default
--                     |> NewColumnConfig.updateBoardType "foo"
--                     |> .columnType
--                     |> Expect.equal "foo"
--         ]
--


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the board name" <|
            \() ->
                NewColumnConfig.default
                    |> NewColumnConfig.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
        ]
