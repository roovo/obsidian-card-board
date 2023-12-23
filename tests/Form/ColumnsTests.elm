module Form.ColumnsTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Column.NamedTag as NamedTagColumn
import Column.OtherTags as OtherTagsColumn
import Column.Undated as UndatedColumn
import Column.Untagged as UntaggedColumn
import Columns
import Expect
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm
import Form.CompletedColumn as CompletedColumnForm
import Form.DatedColumn as DatedColumnForm
import Form.Decoder as FD
import List.Extra as LE
import NewColumnConfig exposing (NewColumnConfig)
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , empty
        , find
        , init
        , optionsForSelect
        , updateColumnName
        , updateCompletedColumnLimit
        , updateDatedColumnRangeType
        , updateDatedColumnRangeValueFrom
        , updateDatedColumnRangeValueTo
        , updateNamedTagTag
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes an empty ColumnsForm" <|
            \() ->
                ColumnsForm.init Columns.empty
                    |> FD.run ColumnsForm.decoder
                    |> Expect.equal (Ok Columns.empty)
        , test "decodes a list of one of each Column type" <|
            \() ->
                { columnForms =
                    [ ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
                    , ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "7" }
                    , ColumnForm.NamedTagColumnForm { name = "foo", tag = "aTag" }
                    , ColumnForm.OtherTagsColumnForm { name = "foo" }
                    , ColumnForm.UndatedColumnForm { name = "foo" }
                    , ColumnForm.UntaggedColumnForm { name = "foo" }
                    ]
                }
                    |> FD.run ColumnsForm.decoder
                    |> Expect.equal
                        (Ok <|
                            Columns.fromList
                                [ Column.Completed <| CompletedColumn.init "foo" 0 4
                                , Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7)
                                , Column.NamedTag <| NamedTagColumn.init "foo" "aTag"
                                , Column.OtherTags <| OtherTagsColumn.init "foo" []
                                , Column.Undated <| UndatedColumn.init "foo"
                                , Column.Untagged <| UntaggedColumn.init "foo"
                                ]
                        )
        , test "errors if given invalid data" <|
            \() ->
                { columnForms =
                    [ ColumnForm.CompletedColumnForm { name = "foo", limit = "p" }
                    , ColumnForm.DatedColumnForm { name = "", rangeType = "Before", from = "", to = "7" }
                    ]
                }
                    |> FD.errors ColumnsForm.decoder
                    |> Expect.equal
                        [ ( 0, ColumnForm.CompletedColumnError (CompletedColumnForm.LimitError CompletedColumnForm.InvalidInt) )
                        , ( 1, ColumnForm.DatedColumnError DatedColumnForm.NameRequired )
                        ]
        ]


empty : Test
empty =
    describe "empty"
        [ test "initialises with empty columnForms" <|
            \() ->
                ColumnsForm.empty
                    |> .columnForms
                    |> Expect.equal []
        ]


find : Test
find =
    describe "find"
        [ test "returns Nothing if there are NO Columns" <|
            \() ->
                { columnForms = [] }
                    |> ColumnsForm.find (always True)
                    |> Expect.equal Nothing
        , test "returns Nothing if there are Columns but none which match the test" <|
            \() ->
                { columnForms =
                    [ ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
                    , ColumnForm.DatedColumnForm { name = "bar", rangeType = "Before", from = "", to = "7" }
                    ]
                }
                    |> ColumnsForm.find (\c -> ColumnForm.name c == "baz")
                    |> Expect.equal Nothing
        , test "returns Just the ColumnForm if there is a matching Column" <|
            \() ->
                { columnForms =
                    [ ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
                    , ColumnForm.DatedColumnForm { name = "bar", rangeType = "Before", from = "", to = "7" }
                    ]
                }
                    |> ColumnsForm.find (\c -> ColumnForm.name c == "foo")
                    |> Expect.equal (Just <| ColumnForm.CompletedColumnForm { name = "foo", limit = "4" })
        ]


init : Test
init =
    describe "init"
        [ test "initialises from an empty Columns" <|
            \() ->
                Columns.empty
                    |> ColumnsForm.init
                    |> .columnForms
                    |> Expect.equal []
        , test "initialises from list of all column types" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7)
                    , Column.NamedTag <| NamedTagColumn.init "foo" "aTag"
                    , Column.OtherTags <| OtherTagsColumn.init "foo" []
                    , Column.Undated <| UndatedColumn.init "foo"
                    , Column.Untagged <| UntaggedColumn.init "foo"
                    ]
                    |> ColumnsForm.init
                    |> .columnForms
                    |> Expect.equal
                        [ ColumnForm.CompletedColumnForm { name = "foo", limit = "5" }
                        , ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "7" }
                        , ColumnForm.NamedTagColumnForm { name = "foo", tag = "aTag" }
                        , ColumnForm.OtherTagsColumnForm { name = "foo" }
                        , ColumnForm.UndatedColumnForm { name = "foo" }
                        , ColumnForm.UntaggedColumnForm { name = "foo" }
                        ]
        ]


optionsForSelect : Test
optionsForSelect =
    describe "optionsForSelect"
        [ test "returns all options if there is an empty columnsForm" <|
            \() ->
                ColumnsForm.optionsForSelect ColumnsForm.empty (NewColumnConfig "" "completed")
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
                ColumnsForm.optionsForSelect ColumnsForm.empty (NewColumnConfig "" "xxx")
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
                    columnsForm : ColumnsForm.Form
                    columnsForm =
                        Columns.fromList
                            [ Column.completed <| CompletedColumn.init "" 0 10 ]
                            |> ColumnsForm.init
                in
                ColumnsForm.optionsForSelect columnsForm (NewColumnConfig "" "dated")
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
                    columnsForm : ColumnsForm.Form
                    columnsForm =
                        Columns.fromList
                            [ Column.otherTags "" [] ]
                            |> ColumnsForm.init
                in
                ColumnsForm.optionsForSelect columnsForm (NewColumnConfig "" "dated")
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
                    columnsForm : ColumnsForm.Form
                    columnsForm =
                        Columns.fromList
                            [ Column.undated "" ]
                            |> ColumnsForm.init
                in
                ColumnsForm.optionsForSelect columnsForm (NewColumnConfig "" "otherTags")
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
                    columnsForm : ColumnsForm.Form
                    columnsForm =
                        Columns.fromList
                            [ Column.untagged "" ]
                            |> ColumnsForm.init
                in
                ColumnsForm.optionsForSelect columnsForm (NewColumnConfig "" "undated")
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


updateColumnName : Test
updateColumnName =
    describe "updateColumnName"
        [ test "does nothing with an empty ColumnsForm" <|
            \() ->
                ColumnsForm.empty
                    |> ColumnsForm.updateColumnName 1 "new name"
                    |> .columnForms
                    |> Expect.equal []
        , test "does nothing if given an index which is past the last one" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.Before 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateColumnName 7 "new name"
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "updates the name at the given index" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.Before 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateColumnName 1 "new name"
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "new name" ]
        ]


updateCompletedColumnLimit : Test
updateCompletedColumnLimit =
    describe "updateCompletedColumnLimit"
        [ test "does nothing with an empty ColumnsForm" <|
            \() ->
                ColumnsForm.empty
                    |> ColumnsForm.updateCompletedColumnLimit 1 "newLimit"
                    |> .columnForms
                    |> Expect.equal []
        , test "does nothing if given an index which is past the last one" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.Before 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateCompletedColumnLimit 7 "newLimit"
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "updates the name at the given index" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.NamedTag <| NamedTagColumn.init "bar" "baz"
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateCompletedColumnLimit 0 "newLimit"
                    |> .columnForms
                    |> LE.getAt 0
                    |> Expect.equal (Just <| ColumnForm.CompletedColumnForm { name = "foo", limit = "newLimit" })
        ]


updateDatedColumnRangeType : Test
updateDatedColumnRangeType =
    describe "updateDatedColumnRangeType"
        [ test "does nothing with an empty ColumnsForm" <|
            \() ->
                ColumnsForm.empty
                    |> ColumnsForm.updateDatedColumnRangeType 1 "newType"
                    |> .columnForms
                    |> Expect.equal []
        , test "does nothing if given an index which is past the last one" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.After 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateDatedColumnRangeType 7 "newType"
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "updates the name at the given index" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.After 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateDatedColumnRangeType 1 "newType"
                    |> .columnForms
                    |> LE.getAt 1
                    |> Expect.equal (Just <| ColumnForm.DatedColumnForm { name = "bar", rangeType = "newType", from = "7", to = "" })
        ]


updateDatedColumnRangeValueFrom : Test
updateDatedColumnRangeValueFrom =
    describe "updateDatedColumnRangeValueFrom"
        [ test "does nothing with an empty ColumnsForm" <|
            \() ->
                ColumnsForm.empty
                    |> ColumnsForm.updateDatedColumnRangeValueFrom 1 "newLimit"
                    |> .columnForms
                    |> Expect.equal []
        , test "does nothing if given an index which is past the last one" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.After 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateDatedColumnRangeValueFrom 7 "newLimit"
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "updates the name at the given index" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.After 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateDatedColumnRangeValueFrom 1 "newValue"
                    |> .columnForms
                    |> LE.getAt 1
                    |> Expect.equal (Just <| ColumnForm.DatedColumnForm { name = "bar", rangeType = "After", from = "newValue", to = "" })
        ]


updateDatedColumnRangeValueTo : Test
updateDatedColumnRangeValueTo =
    describe "updateDatedColumnRangeValueTo"
        [ test "does nothing with an empty ColumnsForm" <|
            \() ->
                ColumnsForm.empty
                    |> ColumnsForm.updateDatedColumnRangeValueTo 1 "newLimit"
                    |> .columnForms
                    |> Expect.equal []
        , test "does nothing if given an index which is past the last one" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.Before 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateDatedColumnRangeValueTo 7 "newLimit"
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "updates the name at the given index" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.Before 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateDatedColumnRangeValueTo 1 "newValue"
                    |> .columnForms
                    |> LE.getAt 1
                    |> Expect.equal (Just <| ColumnForm.DatedColumnForm { name = "bar", rangeType = "Before", from = "", to = "newValue" })
        ]


updateNamedTagTag : Test
updateNamedTagTag =
    describe "updateNamedTagTag"
        [ test "does nothing with an empty ColumnsForm" <|
            \() ->
                ColumnsForm.empty
                    |> ColumnsForm.updateNamedTagTag 1 "newTagName"
                    |> .columnForms
                    |> Expect.equal []
        , test "does nothing if given an index which is past the last one" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.Dated <| DatedColumn.init "bar" (DatedColumn.Before 7)
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateNamedTagTag 7 "newTagName"
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "updates the name at the given index" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "foo" 0 5
                    , Column.NamedTag <| NamedTagColumn.init "bar" "baz"
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.updateNamedTagTag 1 "newTagName"
                    |> .columnForms
                    |> LE.getAt 1
                    |> Expect.equal (Just <| ColumnForm.NamedTagColumnForm { name = "bar", tag = "newTagName" })
        ]
