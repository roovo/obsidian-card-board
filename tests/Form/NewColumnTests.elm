module Form.NewColumnTests exposing (suite)

import Expect
import Form.Column as ColumnForm
import Form.NewColumn as NewColumnForm
import Form.SafeDecoder as SD
import Test exposing (..)


suite : Test
suite =
    concat
        [ default
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
