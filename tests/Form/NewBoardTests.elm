module Form.NewBoardTests exposing (suite)

import Expect
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Test exposing (..)


suite : Test
suite =
    concat
        [ default
        , optionsForSelect

        -- , safeDecoder
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



-- safeDecoder : Test
-- safeDecoder =
--     describe "safeDecoder"
--         [ test "decodes an empty board" <|
--             \() ->
--                 { name = "foo", boardType = "EmptyBoard" }
--                     |> SD.run NewBoardForm.safeDecoder
--                     |> Expect.equal (Ok <| Just (
--
--                       ))
--         ]
-- fromNewBoardConfig : DefaultColumnNames -> NewBoardConfigForm -> BoardConfig
-- fromNewBoardConfig defaultColumnNames newBoardConfigForm =
--     case newBoardConfigForm.boardType of
--         "dateBoard" ->
--             BoardConfig
--                 { columns =
--                     Columns.fromList
--                         [ Column.undated (DefaultColumnNames.nameFor "undated" defaultColumnNames)
--                         , Column.dated <| DatedColumn.init (DefaultColumnNames.nameFor "today" defaultColumnNames) (DatedColumn.Before 1)
--                         , Column.dated <| DatedColumn.init (DefaultColumnNames.nameFor "tomorrow" defaultColumnNames) (DatedColumn.Between { from = 1, to = 1 })
--                         , Column.dated <| DatedColumn.init (DefaultColumnNames.nameFor "future" defaultColumnNames) (DatedColumn.After 1)
--                         , Column.completed <| CompletedColumn.init (DefaultColumnNames.nameFor "completed" defaultColumnNames) 4 10
--                         ]
--                 , filters = []
--                 , filterPolarity = Filter.defaultPolarity
--                 , filterScope = Filter.defaultScope
--                 , name = newBoardConfigForm.name
--                 , showColumnTags = True
--                 , showFilteredTags = True
--                 }
--
--         "tagBoard" ->
--             BoardConfig
--                 { columns =
--                     Columns.fromList
--                         [ Column.untagged (DefaultColumnNames.nameFor "untagged" defaultColumnNames)
--                         , Column.otherTags (DefaultColumnNames.nameFor "otherTags" defaultColumnNames) []
--                         , Column.completed <| CompletedColumn.init (DefaultColumnNames.nameFor "completed" defaultColumnNames) 2 10
--                         ]
--                 , filters = []
--                 , filterPolarity = Filter.defaultPolarity
--                 , filterScope = Filter.defaultScope
--                 , name = newBoardConfigForm.name
--                 , showColumnTags = True
--                 , showFilteredTags = True
--                 }
--
--         _ ->
--             BoardConfig
--                 { columns = Columns.empty
--                 , filters = []
--                 , filterPolarity = Filter.defaultPolarity
--                 , filterScope = Filter.defaultScope
--                 , name = newBoardConfigForm.name
--                 , showColumnTags = True
--                 , showFilteredTags = True
--                 }
-- UTILITIES


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
