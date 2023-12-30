module Form.ColumnTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Column.NamedTag as NamedTagColumn
import Column.OtherTags as OtherTagsColumn
import Column.Undated as UndatedColumn
import Column.Untagged as UntaggedColumn
import DefaultColumnNames
import Expect
import Form.Column as ColumnForm
import Form.SafeDecoder as SD
import Test exposing (..)


suite : Test
suite =
    concat
        [ init
        , name
        , placeholder
        , safeDecoder
        , typeString
        , updateCompletedColumnLimit
        , updateDatedColumnRangeType
        , updateDatedColumnRangeValueFrom
        , updateDatedColumnRangeValueTo
        , updateName
        , updateNamedTagTag
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


placeholder : Test
placeholder =
    describe "placeholder"
        [ test "returns the default name for a completed column" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> ColumnForm.placeholder DefaultColumnNames.default
                    |> Expect.equal "Completed"
        , test "returns an empty string for a dated column" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" }
                    |> ColumnForm.placeholder DefaultColumnNames.default
                    |> Expect.equal ""
        , test "returns an empty string for a dded tagated column" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> ColumnForm.placeholder DefaultColumnNames.default
                    |> Expect.equal ""
        , test "returns the default name for an other tags column" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> ColumnForm.placeholder DefaultColumnNames.default
                    |> Expect.equal "Other Tags"
        , test "returns the default name for an undated column" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> ColumnForm.placeholder DefaultColumnNames.default
                    |> Expect.equal "Undated"
        , test "returns the default name for an untagged column" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> ColumnForm.placeholder DefaultColumnNames.default
                    |> Expect.equal "Untagged"
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


updateCompletedColumnLimit : Test
updateCompletedColumnLimit =
    describe "updateCompletedColumnLimit"
        [ test "updates the column limit of a CompletedColumnForm" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> ColumnForm.updateCompletedColumnLimit "foo"
                    |> Expect.equal
                        (ColumnForm.CompletedColumnForm False
                            { name = "", limit = "foo" }
                        )
        , test "does nothing to a DatedColumnForm" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" }
                    |> ColumnForm.updateCompletedColumnLimit "foo"
                    |> Expect.equal
                        (ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" })
        , test "does nothing to a NamedTagColumnForm" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> ColumnForm.updateCompletedColumnLimit "foo"
                    |> Expect.equal
                        (ColumnForm.NamedTagColumnForm False { name = "", tag = "" })
        , test "does nothing to a OtherTagsColumnForm" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> ColumnForm.updateCompletedColumnLimit "foo"
                    |> Expect.equal
                        (ColumnForm.OtherTagsColumnForm False { name = "" })
        , test "does nothing to a UndatedColumnForm" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> ColumnForm.updateCompletedColumnLimit "foo"
                    |> Expect.equal
                        (ColumnForm.UndatedColumnForm False { name = "" })
        , test "does nothing to a UntaggedColumnForm" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> ColumnForm.updateCompletedColumnLimit "foo"
                    |> Expect.equal
                        (ColumnForm.UntaggedColumnForm False { name = "" })
        ]


updateDatedColumnRangeType : Test
updateDatedColumnRangeType =
    describe "updateDatedColumnRangeType"
        [ test "does nothig to a CompletedColumnForm" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> ColumnForm.updateDatedColumnRangeType "foo"
                    |> Expect.equal
                        (ColumnForm.CompletedColumnForm False { name = "", limit = "" })
        , test "updates the range type for a DatedColumnForm" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" }
                    |> ColumnForm.updateDatedColumnRangeType "foo"
                    |> Expect.equal
                        (ColumnForm.DatedColumnForm False { name = "", rangeType = "foo", from = "", to = "" })
        , test "does nothing to a NamedTagColumnForm" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> ColumnForm.updateDatedColumnRangeType "foo"
                    |> Expect.equal
                        (ColumnForm.NamedTagColumnForm False { name = "", tag = "" })
        , test "does nothing to a OtherTagsColumnForm" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeType "foo"
                    |> Expect.equal
                        (ColumnForm.OtherTagsColumnForm False { name = "" })
        , test "does nothing to a UndatedColumnForm" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeType "foo"
                    |> Expect.equal
                        (ColumnForm.UndatedColumnForm False { name = "" })
        , test "does nothing to a UntaggedColumnForm" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeType "foo"
                    |> Expect.equal
                        (ColumnForm.UntaggedColumnForm False { name = "" })
        ]


updateDatedColumnRangeValueFrom : Test
updateDatedColumnRangeValueFrom =
    describe "updateDatedColumnRangeValueFrom"
        [ test "does nothig to a CompletedColumnForm" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> ColumnForm.updateDatedColumnRangeValueFrom "foo"
                    |> Expect.equal
                        (ColumnForm.CompletedColumnForm False { name = "", limit = "" })
        , test "updates the range type for a DatedColumnForm" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" }
                    |> ColumnForm.updateDatedColumnRangeValueFrom "foo"
                    |> Expect.equal
                        (ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "foo", to = "" })
        , test "does nothing to a NamedTagColumnForm" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> ColumnForm.updateDatedColumnRangeValueFrom "foo"
                    |> Expect.equal
                        (ColumnForm.NamedTagColumnForm False { name = "", tag = "" })
        , test "does nothing to a OtherTagsColumnForm" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeValueFrom "foo"
                    |> Expect.equal
                        (ColumnForm.OtherTagsColumnForm False { name = "" })
        , test "does nothing to a UndatedColumnForm" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeValueFrom "foo"
                    |> Expect.equal
                        (ColumnForm.UndatedColumnForm False { name = "" })
        , test "does nothing to a UntaggedColumnForm" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeValueFrom "foo"
                    |> Expect.equal
                        (ColumnForm.UntaggedColumnForm False { name = "" })
        ]


updateDatedColumnRangeValueTo : Test
updateDatedColumnRangeValueTo =
    describe "updateDatedColumnRangeValueTo"
        [ test "does nothig to a CompletedColumnForm" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> ColumnForm.updateDatedColumnRangeValueTo "foo"
                    |> Expect.equal
                        (ColumnForm.CompletedColumnForm False { name = "", limit = "" })
        , test "updates the range type for a DatedColumnForm" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" }
                    |> ColumnForm.updateDatedColumnRangeValueTo "foo"
                    |> Expect.equal
                        (ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "foo" })
        , test "does nothing to a NamedTagColumnForm" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> ColumnForm.updateDatedColumnRangeValueTo "foo"
                    |> Expect.equal
                        (ColumnForm.NamedTagColumnForm False { name = "", tag = "" })
        , test "does nothing to a OtherTagsColumnForm" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeValueTo "foo"
                    |> Expect.equal
                        (ColumnForm.OtherTagsColumnForm False { name = "" })
        , test "does nothing to a UndatedColumnForm" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeValueTo "foo"
                    |> Expect.equal
                        (ColumnForm.UndatedColumnForm False { name = "" })
        , test "does nothing to a UntaggedColumnForm" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> ColumnForm.updateDatedColumnRangeValueTo "foo"
                    |> Expect.equal
                        (ColumnForm.UntaggedColumnForm False { name = "" })
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name of a CompletedColumnForm" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> ColumnForm.updateName "foo"
                    |> Expect.equal
                        (ColumnForm.CompletedColumnForm False { name = "foo", limit = "" })
        , test "updates the name of a DatedColumnForm" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" }
                    |> ColumnForm.updateName "foo"
                    |> Expect.equal
                        (ColumnForm.DatedColumnForm False { name = "foo", rangeType = "", from = "", to = "" })
        , test "updates the name of a NamedTagColumnForm" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> ColumnForm.updateName "foo"
                    |> Expect.equal
                        (ColumnForm.NamedTagColumnForm False { name = "foo", tag = "" })
        , test "updates the name of a OtherTagsColumnForm" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> ColumnForm.updateName "foo"
                    |> Expect.equal
                        (ColumnForm.OtherTagsColumnForm False { name = "foo" })
        , test "updates the name of a UndatedColumnForm" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> ColumnForm.updateName "foo"
                    |> Expect.equal
                        (ColumnForm.UndatedColumnForm False { name = "foo" })
        , test "updates the name of a UntaggedColumnForm" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> ColumnForm.updateName "foo"
                    |> Expect.equal
                        (ColumnForm.UntaggedColumnForm False { name = "foo" })
        ]


updateNamedTagTag : Test
updateNamedTagTag =
    describe "updateNamedTagTag"
        [ test "does nothig to a CompletedColumnForm" <|
            \() ->
                ColumnForm.CompletedColumnForm False { name = "", limit = "" }
                    |> ColumnForm.updateNamedTagTag "foo"
                    |> Expect.equal
                        (ColumnForm.CompletedColumnForm False { name = "", limit = "" })
        , test "does nothing to a DatedColumnForm" <|
            \() ->
                ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" }
                    |> ColumnForm.updateNamedTagTag "foo"
                    |> Expect.equal
                        (ColumnForm.DatedColumnForm False { name = "", rangeType = "", from = "", to = "" })
        , test "updates the tag of a NamedTagColumnForm" <|
            \() ->
                ColumnForm.NamedTagColumnForm False { name = "", tag = "" }
                    |> ColumnForm.updateNamedTagTag "foo"
                    |> Expect.equal
                        (ColumnForm.NamedTagColumnForm False { name = "", tag = "foo" })
        , test "does nothing to a OtherTagsColumnForm" <|
            \() ->
                ColumnForm.OtherTagsColumnForm False { name = "" }
                    |> ColumnForm.updateNamedTagTag "foo"
                    |> Expect.equal
                        (ColumnForm.OtherTagsColumnForm False { name = "" })
        , test "does nothing to a UndatedColumnForm" <|
            \() ->
                ColumnForm.UndatedColumnForm False { name = "" }
                    |> ColumnForm.updateNamedTagTag "foo"
                    |> Expect.equal
                        (ColumnForm.UndatedColumnForm False { name = "" })
        , test "does nothing to a UntaggedColumnForm" <|
            \() ->
                ColumnForm.UntaggedColumnForm False { name = "" }
                    |> ColumnForm.updateNamedTagTag "foo"
                    |> Expect.equal
                        (ColumnForm.UntaggedColumnForm False { name = "" })
        ]
