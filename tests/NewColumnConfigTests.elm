module NewColumnConfigTests exposing (suite)

import Expect
import NewColumnConfig
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
                NewColumnConfig.default
                    |> .name
                    |> Expect.equal ""
        , test "returns config with an empty columnType" <|
            \() ->
                NewColumnConfig.default
                    |> .columnType
                    |> Expect.equal ""
        ]


updateColumnType : Test
updateColumnType =
    describe "updateColumnType"
        [ test "updates the column type" <|
            \() ->
                NewColumnConfig.default
                    |> NewColumnConfig.updateColumnType "foo"
                    |> .columnType
                    |> Expect.equal "foo"
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the board name" <|
            \() ->
                NewColumnConfig.default
                    |> NewColumnConfig.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
        ]
