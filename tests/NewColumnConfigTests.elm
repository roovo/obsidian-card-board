module NewColumnConfigTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Columns
import DefaultColumnNames
import Expect
import NewColumnConfig exposing (NewColumnConfig)
import Test exposing (..)


suite : Test
suite =
    concat
        [ default
        , optionsForSelect

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


optionsForSelect : Test
optionsForSelect =
    describe "optionsForSelect"
        [ test "returns all options if there are no columns" <|
            \() ->
                NewColumnConfig.optionsForSelect Columns.empty (NewColumnConfig "" "completed")
                    |> Expect.equal
                        [ { isSelected = True
                          , text = "Completed"
                          , value = "completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "otherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "namedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "untagged"
                          }
                        ]
        , test "selects the first item if the selection isn't valid" <|
            \() ->
                NewColumnConfig.optionsForSelect Columns.empty (NewColumnConfig "" "xxx")
                    |> Expect.equal
                        [ { isSelected = True
                          , text = "Completed"
                          , value = "completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "otherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "namedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "untagged"
                          }
                        ]
        , test "returns all options except Completed if there is a completed column" <|
            \() ->
                let
                    columns =
                        Columns.fromList
                            [ Column.completed <| CompletedColumn.init "" 0 10 ]
                in
                NewColumnConfig.optionsForSelect columns (NewColumnConfig "" "dated")
                    |> Expect.equal
                        [ { isSelected = True
                          , text = "Dated"
                          , value = "dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "otherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "namedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "untagged"
                          }
                        ]
        , test "returns all options except otherTags if there is an otherTags column" <|
            \() ->
                let
                    columns =
                        Columns.fromList
                            [ Column.otherTags "" [] ]
                in
                NewColumnConfig.optionsForSelect columns (NewColumnConfig "" "dated")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Completed"
                          , value = "completed"
                          }
                        , { isSelected = True
                          , text = "Dated"
                          , value = "dated"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "namedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "untagged"
                          }
                        ]
        , test "returns all options except undated if there is an undated column" <|
            \() ->
                let
                    columns =
                        Columns.fromList
                            [ Column.undated "" ]
                in
                NewColumnConfig.optionsForSelect columns (NewColumnConfig "" "otherTags")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Completed"
                          , value = "completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "dated"
                          }
                        , { isSelected = True
                          , text = "Other Tags"
                          , value = "otherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "namedTag"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "untagged"
                          }
                        ]
        , test "returns all options except untagged if there is an untagged column" <|
            \() ->
                let
                    columns =
                        Columns.fromList
                            [ Column.untagged "" ]
                in
                NewColumnConfig.optionsForSelect columns (NewColumnConfig "" "undated")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Completed"
                          , value = "completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "otherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "namedTag"
                          }
                        , { isSelected = True
                          , text = "Undated"
                          , value = "undated"
                          }
                        ]

        -- updateBoardType : Test
        -- updateBoardType =
        --     describe "updateBoardType"
        --         [ test "updates the board name" <|
        --             \() ->
        --                 NewColumnConfig.default
        --                     |> NewColumnConfig.updateBoardType "foo"
        --                     |> .columnType
        --                     |> Expect.equal "foo"
        ]


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
