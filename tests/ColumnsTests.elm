module ColumnsTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Columns
import DefaultColumnNames
import Expect
import NewColumnConfig exposing (NewColumnConfig)
import Test exposing (..)


suite : Test
suite =
    concat
        [ fromList
        , optionsForSelect
        ]


fromList : Test
fromList =
    describe "fromList"
        [ test "works with an empty list" <|
            \() ->
                []
                    |> Columns.fromList
                    |> Columns.toList
                    |> Expect.equal []
        , test "works with a list containing a single column" <|
            \() ->
                [ Column.otherTags "others" [] ]
                    |> Columns.fromList
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "others" ]
        , test "preserves columns order" <|
            \() ->
                [ Column.otherTags "others" []
                , Column.undated "undated"
                , Column.dated <| DatedColumn.init "today" (DatedColumn.Before 1)
                , Column.untagged "untagged"
                , Column.completed <| CompletedColumn.init "completed" 1 10
                , Column.namedTag "tagged" "aTag"
                ]
                    |> Columns.fromList
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "others", "undated", "today", "untagged", "completed", "tagged" ]
        ]


optionsForSelect : Test
optionsForSelect =
    describe "optionsForSelect"
        [ test "returns all options if there are no columns" <|
            \() ->
                Columns.optionsForSelect Columns.empty (NewColumnConfig "" "completed")
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
                Columns.optionsForSelect Columns.empty (NewColumnConfig "" "xxx")
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
                Columns.optionsForSelect columns (NewColumnConfig "" "dated")
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
                Columns.optionsForSelect columns (NewColumnConfig "" "dated")
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
                Columns.optionsForSelect columns (NewColumnConfig "" "otherTags")
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
                Columns.optionsForSelect columns (NewColumnConfig "" "undated")
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
        ]
