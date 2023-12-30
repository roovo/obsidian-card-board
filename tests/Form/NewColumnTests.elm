module Form.NewColumnTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Columns
import Expect
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm exposing (ColumnsForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.SafeDecoder as SD
import Test exposing (..)


suite : Test
suite =
    concat
        [ default
        , optionsForSelect
        , safeDecoder
        , updateColumnType
        , updateName
        ]


default : Test
default =
    describe "default"
        [ test "returns config with no name" <|
            \() ->
                NewColumnForm.default
                    |> .name
                    |> Expect.equal ""
        , test "returns config with an empty columnType" <|
            \() ->
                NewColumnForm.default
                    |> .columnType
                    |> Expect.equal ""
        ]


optionsForSelect : Test
optionsForSelect =
    describe "optionsForSelect"
        [ test "returns all options if there is an empty columnsForm" <|
            \() ->
                NewColumnForm.optionsForSelect ColumnsForm.empty (NewColumnForm "" "completed")
                    |> Expect.equal
                        [ { isSelected = True
                          , text = "Completed"
                          , value = "Completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "Dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "OtherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "NamedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "Undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "Untagged"
                          }
                        ]
        , test "selects the first item if the selection isn't valid" <|
            \() ->
                NewColumnForm.optionsForSelect ColumnsForm.empty (NewColumnForm "" "xxx")
                    |> Expect.equal
                        [ { isSelected = True
                          , text = "Completed"
                          , value = "Completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "Dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "OtherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "NamedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "Undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "Untagged"
                          }
                        ]
        , test "returns all options except Completed if there is a completed column" <|
            \() ->
                let
                    columnsForm : ColumnsForm
                    columnsForm =
                        Columns.fromList
                            [ Column.completed <| CompletedColumn.init "" 0 10 ]
                            |> ColumnsForm.init
                in
                NewColumnForm.optionsForSelect columnsForm (NewColumnForm "" "dated")
                    |> Expect.equal
                        [ { isSelected = True
                          , text = "Dated"
                          , value = "Dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "OtherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "NamedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "Undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "Untagged"
                          }
                        ]
        , test "returns all options except otherTags if there is an otherTags column" <|
            \() ->
                let
                    columnsForm : ColumnsForm
                    columnsForm =
                        Columns.fromList
                            [ Column.otherTags "" [] ]
                            |> ColumnsForm.init
                in
                NewColumnForm.optionsForSelect columnsForm (NewColumnForm "" "dated")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Completed"
                          , value = "Completed"
                          }
                        , { isSelected = True
                          , text = "Dated"
                          , value = "Dated"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "NamedTag"
                          }
                        , { isSelected = False
                          , text = "Undated"
                          , value = "Undated"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "Untagged"
                          }
                        ]
        , test "returns all options except undated if there is an undated column" <|
            \() ->
                let
                    columnsForm : ColumnsForm
                    columnsForm =
                        Columns.fromList
                            [ Column.undated "" ]
                            |> ColumnsForm.init
                in
                NewColumnForm.optionsForSelect columnsForm (NewColumnForm "" "otherTags")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Completed"
                          , value = "Completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "Dated"
                          }
                        , { isSelected = True
                          , text = "Other Tags"
                          , value = "OtherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "NamedTag"
                          }
                        , { isSelected = False
                          , text = "Untagged"
                          , value = "Untagged"
                          }
                        ]
        , test "returns all options except untagged if there is an untagged column" <|
            \() ->
                let
                    columnsForm : ColumnsForm
                    columnsForm =
                        Columns.fromList
                            [ Column.untagged "" ]
                            |> ColumnsForm.init
                in
                NewColumnForm.optionsForSelect columnsForm (NewColumnForm "" "undated")
                    |> Expect.equal
                        [ { isSelected = False
                          , text = "Completed"
                          , value = "Completed"
                          }
                        , { isSelected = False
                          , text = "Dated"
                          , value = "Dated"
                          }
                        , { isSelected = False
                          , text = "Other Tags"
                          , value = "OtherTags"
                          }
                        , { isSelected = False
                          , text = "Tagged"
                          , value = "NamedTag"
                          }
                        , { isSelected = True
                          , text = "Undated"
                          , value = "Undated"
                          }
                        ]
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes as an uncollapsed Completed column" <|
            \() ->
                { name = "foo", columnType = "Completed" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.CompletedColumnForm False { name = "foo", limit = "10" }))
        , test "decodes as an uncollapsed Dated column" <|
            \() ->
                { name = "foo", columnType = "Dated" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.DatedColumnForm False { name = "foo", rangeType = "Before", from = "", to = "" }))
        , test "decodes as an uncollapsed NamedTag column" <|
            \() ->
                { name = "foo", columnType = "NamedTag" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.NamedTagColumnForm False { name = "foo", tag = "" }))
        , test "decodes as an uncollapsed OtherTags column" <|
            \() ->
                { name = "foo", columnType = "OtherTags" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.OtherTagsColumnForm False { name = "foo" }))
        , test "decodes as an uncollapsed Undated column" <|
            \() ->
                { name = "foo", columnType = "Undated" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.UndatedColumnForm False { name = "foo" }))
        , test "decodes as an uncollapsed Untagged column" <|
            \() ->
                { name = "foo", columnType = "Untagged" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.UntaggedColumnForm False { name = "foo" }))
        , test "decodes an INVALID column" <|
            \() ->
                { name = "foo", columnType = "XXXXXXXXXX" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok Nothing)
        ]


updateColumnType : Test
updateColumnType =
    describe "updateColumnType"
        [ test "updates the column type" <|
            \() ->
                NewColumnForm.default
                    |> NewColumnForm.updateColumnType "foo"
                    |> .columnType
                    |> Expect.equal "foo"
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the board name" <|
            \() ->
                NewColumnForm.default
                    |> NewColumnForm.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
        ]
