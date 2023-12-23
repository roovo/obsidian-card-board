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
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , empty
        , find
        , init
        , updateColumnName
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
