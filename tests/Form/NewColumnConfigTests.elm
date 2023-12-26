module Form.NewColumnConfigTests exposing (suite)

import Expect
import Form.NewColumnConfig as NewColumnConfigForm
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
                NewColumnConfigForm.default
                    |> .name
                    |> Expect.equal ""
        , test "returns config with an empty columnType" <|
            \() ->
                NewColumnConfigForm.default
                    |> .columnType
                    |> Expect.equal ""
        ]


updateColumnType : Test
updateColumnType =
    describe "updateColumnType"
        [ test "updates the column type" <|
            \() ->
                NewColumnConfigForm.default
                    |> NewColumnConfigForm.updateColumnType "foo"
                    |> .columnType
                    |> Expect.equal "foo"
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the board name" <|
            \() ->
                NewColumnConfigForm.default
                    |> NewColumnConfigForm.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
        ]
