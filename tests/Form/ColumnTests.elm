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
import Form.SafeDecoder as SD
import Test exposing (..)


suite : Test
suite =
    concat
        [ init
        , name
        , safeDecoder
        , typeString
        ]


init : Test
init =
    describe "init"
        [ test "initialises from a Completed column" <|
            \() ->
                (Column.Completed <| CompletedColumn.init "foo" 0 7)
                    |> Column.setCollapse False
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.CompletedColumnForm False { name = "foo", limit = "7" })
        , test "initialises from a collapsed Completed column" <|
            \() ->
                (Column.Completed <| CompletedColumn.init "foo" 0 7)
                    |> Column.setCollapse True
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.CompletedColumnForm True { name = "foo", limit = "7" })
        , test "initialises from a Dated column" <|
            \() ->
                (Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7))
                    |> Column.setCollapse False
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.DatedColumnForm False { name = "foo", rangeType = "Before", from = "", to = "7" })
        , test "initialises from a collaped Dated column" <|
            \() ->
                (Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7))
                    |> Column.setCollapse True
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.DatedColumnForm True { name = "foo", rangeType = "Before", from = "", to = "7" })
        , test "initialises from a NamedTag column" <|
            \() ->
                (Column.NamedTag <| NamedTagColumn.init "foo" "aTag")
                    |> Column.setCollapse False
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.NamedTagColumnForm False { name = "foo", tag = "aTag" })
        , test "initialises from a collaped NamedTag column" <|
            \() ->
                (Column.NamedTag <| NamedTagColumn.init "foo" "aTag")
                    |> Column.setCollapse True
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.NamedTagColumnForm True { name = "foo", tag = "aTag" })
        , test "initialises from an OtherTags column" <|
            \() ->
                (Column.OtherTags <| OtherTagsColumn.init "foo" [])
                    |> Column.setCollapse False
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.OtherTagsColumnForm False { name = "foo" })
        , test "initialises from a collaped OtherTags column" <|
            \() ->
                (Column.OtherTags <| OtherTagsColumn.init "foo" [])
                    |> Column.setCollapse True
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.OtherTagsColumnForm True { name = "foo" })
        , test "initialises from an Undated column" <|
            \() ->
                (Column.Undated <| UndatedColumn.init "foo")
                    |> Column.setCollapse False
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.UndatedColumnForm False { name = "foo" })
        , test "initialises from a collaped Undated column" <|
            \() ->
                (Column.Undated <| UndatedColumn.init "foo")
                    |> Column.setCollapse True
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.UndatedColumnForm True { name = "foo" })
        , test "initialises from an Untagged column" <|
            \() ->
                (Column.Untagged <| UntaggedColumn.init "foo")
                    |> Column.setCollapse False
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.UntaggedColumnForm False { name = "foo" })
        , test "initialises from a collaped Untagged column" <|
            \() ->
                (Column.Untagged <| UntaggedColumn.init "foo")
                    |> Column.setCollapse True
                    |> ColumnForm.init
                    |> Expect.equal (ColumnForm.UntaggedColumnForm True { name = "foo" })
        ]


name : Test
name =
    describe "name"
        [ test "returns the name of a Completed column" <|
            \() ->
                (Column.Completed <| CompletedColumn.init "foo" 0 7)
                    |> ColumnForm.init
                    |> ColumnForm.name
                    |> Expect.equal "foo"
        , test "returns the name of a Dated column" <|
            \() ->
                (Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7))
                    |> ColumnForm.init
                    |> ColumnForm.name
                    |> Expect.equal "foo"
        , test "returns the name of a NamedTag column" <|
            \() ->
                (Column.NamedTag <| NamedTagColumn.init "foo" "aTag")
                    |> ColumnForm.init
                    |> ColumnForm.name
                    |> Expect.equal "foo"
        , test "returns the name of an OtherTags column" <|
            \() ->
                (Column.OtherTags <| OtherTagsColumn.init "foo" [])
                    |> ColumnForm.init
                    |> ColumnForm.name
                    |> Expect.equal "foo"
        , test "returns the name of an Undated column" <|
            \() ->
                (Column.Undated <| UndatedColumn.init "foo")
                    |> ColumnForm.init
                    |> ColumnForm.name
                    |> Expect.equal "foo"
        , test "returns the name of an Untagged column" <|
            \() ->
                (Column.Untagged <| UntaggedColumn.init "foo")
                    |> ColumnForm.init
                    |> ColumnForm.name
                    |> Expect.equal "foo"
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes a valid Completed form" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "foo", limit = "4" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.completed (CompletedColumn.init "foo" 0 4))
        , test "decodes a valid collapsed Completed form" <|
            \() ->
                ColumnForm.CompletedColumnForm True { name = "foo", limit = "4" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Result.map Column.isCollapsed
                    |> Expect.equal (Ok True)
        , test "allows an invalid Completed form" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.completed (CompletedColumn.init "" 0 10))
        , test "decodes a valid Dated form" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "foo", rangeType = "After", from = "2", to = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.dated (DatedColumn.init "foo" (DatedColumn.After 2)))
        , test "decodes a valid collapsed Dated form" <|
            \() ->
                ColumnForm.DatedColumnForm True { name = "foo", rangeType = "After", from = "2", to = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Result.map Column.isCollapsed
                    |> Expect.equal (Ok True)
        , test "allows an invalid Dated form" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "Before", from = "2", to = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.dated (DatedColumn.init "" (DatedColumn.Before 0)))
        , test "decodes a valid NamedTag form" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "foo", tag = "aTag" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.namedTag "foo" "aTag")
        , test "decodes a valid collapsed NamedTag form" <|
            \() ->
                ColumnForm.NamedTagColumnForm True { name = "foo", tag = "aTag" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Result.map Column.isCollapsed
                    |> Expect.equal (Ok True)
        , test "allows an invalid NamedTag form" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.namedTag "" "")
        , test "decodes a valid OtherTags form" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "foo" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.otherTags "foo" [])
        , test "decodes a valid collapsed OtherTags form" <|
            \() ->
                ColumnForm.OtherTagsColumnForm True { name = "foo" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Result.map Column.isCollapsed
                    |> Expect.equal (Ok True)
        , test "allows an invalid OtherTags form" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.otherTags "" [])
        , test "decodes a valid Undated form" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "foo" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.undated "foo")
        , test "decodes a valid collapsed Undated form" <|
            \() ->
                ColumnForm.UndatedColumnForm True { name = "foo" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Result.map Column.isCollapsed
                    |> Expect.equal (Ok True)
        , test "allows an invalid Undated form" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.undated "")
        , test "decodes a valid Untagged form" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "foo" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.untagged "foo")
        , test "decodes a valid collapsed Untagged form" <|
            \() ->
                ColumnForm.UntaggedColumnForm True { name = "foo" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Result.map Column.isCollapsed
                    |> Expect.equal (Ok True)
        , test "allows an invalid Untagged form" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> SD.run ColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Column.untagged "")
        ]


typeString : Test
typeString =
    describe "typeString"
        [ test "returns 'Completed' for a Completed column" <|
            \() ->
                (Column.Completed <| CompletedColumn.init "foo" 0 7)
                    |> ColumnForm.init
                    |> ColumnForm.typeString
                    |> Expect.equal "Completed"
        , test "returns 'Dated' for a Dated column" <|
            \() ->
                (Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7))
                    |> ColumnForm.init
                    |> ColumnForm.typeString
                    |> Expect.equal "Dated"
        , test "returns 'Tagged' for a NamedTag column" <|
            \() ->
                (Column.NamedTag <| NamedTagColumn.init "foo" "aTag")
                    |> ColumnForm.init
                    |> ColumnForm.typeString
                    |> Expect.equal "Tagged"
        , test "returns 'Other Tags' for an OtherTags column" <|
            \() ->
                (Column.OtherTags <| OtherTagsColumn.init "foo" [])
                    |> ColumnForm.init
                    |> ColumnForm.typeString
                    |> Expect.equal "Other Tags"
        , test "returns 'Undated' for an Undated column" <|
            \() ->
                (Column.Undated <| UndatedColumn.init "foo")
                    |> ColumnForm.init
                    |> ColumnForm.typeString
                    |> Expect.equal "Undated"
        , test "returns 'Untagged' for an Untagged column" <|
            \() ->
                (Column.Untagged <| UntaggedColumn.init "foo")
                    |> ColumnForm.init
                    |> ColumnForm.typeString
                    |> Expect.equal "Untagged"
        ]
