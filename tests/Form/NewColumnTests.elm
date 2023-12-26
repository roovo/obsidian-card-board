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
        [ test "decodes a Completed column" <|
            \() ->
                { name = "foo", columnType = "Completed" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.CompletedColumnForm { name = "foo", limit = "10" }))
        , test "decodes a Dated column" <|
            \() ->
                { name = "foo", columnType = "Dated" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "" }))
        , test "decodes a NamedTag column" <|
            \() ->
                { name = "foo", columnType = "NamedTag" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.NamedTagColumnForm { name = "foo", tag = "" }))
        , test "decodes an OtherTags column" <|
            \() ->
                { name = "foo", columnType = "OtherTags" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.OtherTagsColumnForm { name = "foo" }))
        , test "decodes an Undated column" <|
            \() ->
                { name = "foo", columnType = "Undated" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.UndatedColumnForm { name = "foo" }))
        , test "decodes an Untagged column" <|
            \() ->
                { name = "foo", columnType = "Untagged" }
                    |> SD.run NewColumnForm.safeDecoder
                    |> Expect.equal (Ok <| Just (ColumnForm.UntaggedColumnForm { name = "foo" }))
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
