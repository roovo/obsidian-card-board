module Form.ColumnsTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Column.NamedTag as NamedTagColumn
import Column.OtherTags as OtherTagsColumn
import Column.Undated as UndatedColumn
import Column.Untagged as UntaggedColumn
import Columns
import DragAndDrop.BeaconPosition as BeaconPosition
import Expect
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm
import Form.SafeDecoder as SD
import List.Extra as LE
import Test exposing (..)


suite : Test
suite =
    concat
        [ addColumn
        , empty
        , find
        , init
        , moveColumn
        , safeDecoder
        , updateColumnName
        , updateCompletedColumnLimit
        , updateDatedColumnRangeType
        , updateDatedColumnRangeValueFrom
        , updateDatedColumnRangeValueTo
        , updateNamedTagTag
        ]


addColumn : Test
addColumn =
    describe "addColumn"
        [ test "does nothing if there is no column to add (it's Nothing)" <|
            \() ->
                ColumnsForm.addColumn Nothing ColumnsForm.empty
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal []
        , test "adds an Undated columnn to an empty ColumnsForm" <|
            \() ->
                ColumnsForm.addColumn
                    (Just <| ColumnForm.UndatedColumnForm False { name = "foo" })
                    ColumnsForm.empty
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo" ]
        , test "adds an Undated columnn after an existing Untagged column" <|
            \() ->
                ColumnsForm.addColumn
                    (Just <| ColumnForm.UndatedColumnForm False { name = "bar" })
                    { columnForms = [ ColumnForm.UntaggedColumnForm False { name = "foo" } ] }
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "adds a non-Completed columnn after the completed column" <|
            \() ->
                ColumnsForm.addColumn
                    (Just <| ColumnForm.UndatedColumnForm False { name = "baz" })
                    { columnForms =
                        [ ColumnForm.UntaggedColumnForm False { name = "foo" }
                        , ColumnForm.CompletedColumnForm False { name = "bar", limit = "5" }
                        ]
                    }
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar", "baz" ]
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
                    [ ColumnForm.CompletedColumnForm False { name = "foo", limit = "4" }
                    , ColumnForm.DatedColumnForm False { name = "bar", rangeType = "Before", from = "", to = "7" }
                    ]
                }
                    |> ColumnsForm.find (\c -> ColumnForm.name c == "baz")
                    |> Expect.equal Nothing
        , test "returns Just the ColumnForm if there is a matching Column" <|
            \() ->
                { columnForms =
                    [ ColumnForm.CompletedColumnForm False { name = "foo", limit = "4" }
                    , ColumnForm.DatedColumnForm False { name = "bar", rangeType = "Before", from = "", to = "7" }
                    ]
                }
                    |> ColumnsForm.find (\c -> ColumnForm.name c == "foo")
                    |> Expect.equal (Just <| ColumnForm.CompletedColumnForm False { name = "foo", limit = "4" })
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
                        [ ColumnForm.CompletedColumnForm False { name = "foo", limit = "5" }
                        , ColumnForm.DatedColumnForm False { name = "foo", rangeType = "Before", from = "", to = "7" }
                        , ColumnForm.NamedTagColumnForm False { name = "foo", tag = "aTag" }
                        , ColumnForm.OtherTagsColumnForm False { name = "foo" }
                        , ColumnForm.UndatedColumnForm False { name = "foo" }
                        , ColumnForm.UntaggedColumnForm False { name = "foo" }
                        ]
        ]


moveColumn : Test
moveColumn =
    describe "moveColumn"
        [ test "does nothing if there are no columns" <|
            \() ->
                ColumnsForm.init Columns.empty
                    |> ColumnsForm.moveColumn "0" (BeaconPosition.After "0")
                    |> .columnForms
                    |> Expect.equal []
        , test "moves a column" <|
            \() ->
                Columns.fromList
                    [ Column.Completed <| CompletedColumn.init "one" 0 5
                    , Column.Dated <| DatedColumn.init "two" (DatedColumn.Before 7)
                    , Column.NamedTag <| NamedTagColumn.init "three" "aTag"
                    ]
                    |> ColumnsForm.init
                    |> ColumnsForm.moveColumn "one" (BeaconPosition.After "two")
                    |> .columnForms
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "two", "one", "three" ]
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes an empty ColumnsForm" <|
            \() ->
                ColumnsForm.init Columns.empty
                    |> SD.run ColumnsForm.safeDecoder
                    |> Expect.equal (Ok Columns.empty)
        , test "decodes a list of one of each Column type" <|
            \() ->
                { columnForms =
                    [ ColumnForm.CompletedColumnForm False { name = "foo", limit = "4" }
                    , ColumnForm.DatedColumnForm False { name = "foo", rangeType = "Before", from = "", to = "7" }
                    , ColumnForm.NamedTagColumnForm False { name = "foo", tag = "aTag" }
                    , ColumnForm.OtherTagsColumnForm False { name = "foo" }
                    , ColumnForm.UndatedColumnForm False { name = "foo" }
                    , ColumnForm.UntaggedColumnForm False { name = "foo" }
                    ]
                }
                    |> SD.run ColumnsForm.safeDecoder
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
        , test "decodes if given invalid data" <|
            \() ->
                { columnForms =
                    [ ColumnForm.CompletedColumnForm False { name = "foo", limit = "p" }
                    , ColumnForm.DatedColumnForm False { name = "", rangeType = "Before", from = "", to = "plop" }
                    ]
                }
                    |> SD.run ColumnsForm.safeDecoder
                    |> Expect.equal
                        (Ok <|
                            Columns.fromList
                                [ Column.Completed <| CompletedColumn.init "foo" 0 10
                                , Column.Dated <| DatedColumn.init "" (DatedColumn.Before 0)
                                ]
                        )
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
                    |> Expect.equal (Just <| ColumnForm.CompletedColumnForm False { name = "foo", limit = "newLimit" })
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
                    |> Expect.equal (Just <| ColumnForm.DatedColumnForm False { name = "bar", rangeType = "newType", from = "7", to = "" })
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
                    |> Expect.equal (Just <| ColumnForm.DatedColumnForm False { name = "bar", rangeType = "After", from = "newValue", to = "" })
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
                    |> Expect.equal (Just <| ColumnForm.DatedColumnForm False { name = "bar", rangeType = "Before", from = "", to = "newValue" })
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
                    |> Expect.equal (Just <| ColumnForm.NamedTagColumnForm False { name = "bar", tag = "newTagName" })
        ]
