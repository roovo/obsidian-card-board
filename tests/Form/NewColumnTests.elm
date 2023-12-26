module Form.NewColumnTests exposing (suite)

import Expect
import Form.NewColumn as NewColumnForm
import Test exposing (..)


suite : Test
suite =
    concat
        [ default
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
