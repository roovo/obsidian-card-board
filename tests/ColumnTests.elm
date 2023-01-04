module ColumnTests exposing (suite)

import Column
import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ name
        , isEmpty
        , items
        , hasName
        ]


name : Test
name =
    describe "name"
        [ test "returns the name of the column" <|
            \() ->
                Column.init "a name" []
                    |> Column.name
                    |> Expect.equal "a name"
        ]


isEmpty : Test
isEmpty =
    describe "isEmpty"
        [ test "returns True for an empty Column" <|
            \() ->
                Column.init "a name" []
                    |> Column.isEmpty
                    |> Expect.equal True
        , test "returns False for a non empty Column" <|
            \() ->
                Column.init "a name" [ 1 ]
                    |> Column.isEmpty
                    |> Expect.equal False
        ]


items : Test
items =
    describe "items"
        [ test "returns the items in the column" <|
            \() ->
                Column.init "a name" [ "item 1", "item 2" ]
                    |> Column.items
                    |> Expect.equal [ "item 1", "item 2" ]
        ]


hasName : Test
hasName =
    describe "hasName"
        [ test "returns True if the colum name matches" <|
            \() ->
                Column.init "a name" []
                    |> Column.hasName "a name"
                    |> Expect.equal True
        , test "returns False if the colum name does not match" <|
            \() ->
                Column.init "a name" []
                    |> Column.hasName "A NAME"
                    |> Expect.equal False
        ]
