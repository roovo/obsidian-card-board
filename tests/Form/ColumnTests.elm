module Form.ColumnTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Column.NamedTag as NamedTagColumn
import Column.OtherTags as OtherTagsColumn
import Column.Undated as UndatedColumn
import Column.Untagged as UntaggedColumn
import Expect
import Form.Column as ColumnForm
import Form.CompletedColumn as CompletedColumnForm
import Form.DatedColumn as DatedColumnForm
import Form.Decoder as FD
import Form.NamedTagColumn as NamedTagColumnForm
import Form.OtherTagsColumn as OtherTagsColumnForm
import Form.UndatedColumn as UndatedColumnForm
import Form.UntaggedColumn as UntaggedColumnForm
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , init
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes a valid Completed SubForm" <|
            \() ->
                ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
                    |> FD.run ColumnForm.decoder
                    |> Expect.equal (Ok <| Column.completed (CompletedColumn.init "foo" 0 4))
        , test "errors an invalid Completed form" <|
            \() ->
                ColumnForm.CompletedColumnForm { name = "", limit = "4" }
                    |> FD.errors ColumnForm.decoder
                    |> Expect.equal [ ColumnForm.CompletedColumnError CompletedColumnForm.NameRequired ]
        , test "decodes a valid Dated form" <|
            \() ->
                ColumnForm.DatedColumnForm { name = "foo", rangeType = "After", from = "", to = "2" }
                    |> FD.run ColumnForm.decoder
                    |> Expect.equal (Ok <| Column.dated (DatedColumn.init "foo" (DatedColumn.After 2)))
        , test "errors an invalid Dated form" <|
            \() ->
                ColumnForm.DatedColumnForm { name = "", rangeType = "After", from = "", to = "2" }
                    |> FD.errors ColumnForm.decoder
                    |> Expect.equal [ ColumnForm.DatedColumnError DatedColumnForm.NameRequired ]
        , test "decodes a valid NamedTag form" <|
            \() ->
                ColumnForm.NamedTagColumnForm { name = "foo", tag = "aTag" }
                    |> FD.run ColumnForm.decoder
                    |> Expect.equal (Ok <| Column.namedTag "foo" "aTag")
        , test "errors an invalid NamedTag form" <|
            \() ->
                ColumnForm.NamedTagColumnForm { name = "", tag = "aTag" }
                    |> FD.errors ColumnForm.decoder
                    |> Expect.equal [ ColumnForm.NamedTagColumnError NamedTagColumnForm.NameRequired ]
        , test "decodes a valid OtherTags form" <|
            \() ->
                ColumnForm.OtherTagsColumnForm { name = "foo" }
                    |> FD.run ColumnForm.decoder
                    |> Expect.equal (Ok <| Column.otherTags "foo" [])
        , test "errors an invalid OtherTags form" <|
            \() ->
                ColumnForm.OtherTagsColumnForm { name = "" }
                    |> FD.errors ColumnForm.decoder
                    |> Expect.equal [ ColumnForm.OtherTagsColumnError OtherTagsColumnForm.NameRequired ]
        , test "decodes a valid Undated form" <|
            \() ->
                ColumnForm.UndatedColumnForm { name = "foo" }
                    |> FD.run ColumnForm.decoder
                    |> Expect.equal (Ok <| Column.undated "foo")
        , test "errors an invalid Undated form" <|
            \() ->
                ColumnForm.UndatedColumnForm { name = "" }
                    |> FD.errors ColumnForm.decoder
                    |> Expect.equal [ ColumnForm.UndatedColumnError UndatedColumnForm.NameRequired ]
        , test "decodes a valid Untagged form" <|
            \() ->
                ColumnForm.UntaggedColumnForm { name = "foo" }
                    |> FD.run ColumnForm.decoder
                    |> Expect.equal (Ok <| Column.untagged "foo")
        , test "errors an invalid Untagged form" <|
            \() ->
                ColumnForm.UntaggedColumnForm { name = "" }
                    |> FD.errors ColumnForm.decoder
                    |> Expect.equal [ ColumnForm.UntaggedColumnError UntaggedColumnForm.NameRequired ]
        ]


init : Test
init =
    describe "init"
        [ test "initialises from a Completed column" <|
            \() ->
                (Column.Completed <| CompletedColumn.init "foo" 0 7)
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.CompletedColumnForm { name = "foo", limit = "7" })
        , test "initialises from a Dated column" <|
            \() ->
                (Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7))
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "7" })
        , test "initialises from a NamedTag column" <|
            \() ->
                (Column.NamedTag <| NamedTagColumn.init "foo" "aTag")
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.NamedTagColumnForm { name = "foo", tag = "aTag" })
        , test "initialises from an OtherTags column" <|
            \() ->
                (Column.OtherTags <| OtherTagsColumn.init "foo" [])
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.OtherTagsColumnForm { name = "foo" })
        , test "initialises from an Undated column" <|
            \() ->
                (Column.Undated <| UndatedColumn.init "foo")
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.UndatedColumnForm { name = "foo" })
        , test "initialises from an Untagged column" <|
            \() ->
                (Column.Untagged <| UntaggedColumn.init "foo")
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.UntaggedColumnForm { name = "foo" })
        ]
