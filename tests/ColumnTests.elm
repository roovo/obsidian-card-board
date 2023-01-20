module ColumnTests exposing (suite)

import Column
import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ name
        , isCollapsed
        , isEmpty
        , isEnabled
        , items
        , hasName
        ]


name : Test
name =
    describe "name"
        [ test "returns the name of the column" <|
            \() ->
                Column.init True "a name" []
                    |> Column.name
                    |> Expect.equal "a name"
        ]


isCollapsed : Test
isCollapsed =
    describe "isCollapsed"
        [ test "returns True for an initialised empty Column" <|
            \() ->
                Column.init True "a name" []
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "returns False for an initialised non-empty Column" <|
            \() ->
                Column.init True "a name" [ 1 ]
                    |> Column.isCollapsed
                    |> Expect.equal False
        ]


isEmpty : Test
isEmpty =
    describe "isEmpty"
        [ test "returns True for an empty Column" <|
            \() ->
                Column.init True "a name" []
                    |> Column.isEmpty
                    |> Expect.equal True
        , test "returns False for a non empty Column" <|
            \() ->
                Column.init True "a name" [ 1 ]
                    |> Column.isEmpty
                    |> Expect.equal False
        ]


isEnabled : Test
isEnabled =
    describe "isEnabled"
        [ test "returns True if intialized as enabled" <|
            \() ->
                Column.init True "name" []
                    |> Column.isEnabled
                    |> Expect.equal True
        , test "returns False if intialized as enabled" <|
            \() ->
                Column.init False "name" []
                    |> Column.isEnabled
                    |> Expect.equal False
        ]


items : Test
items =
    describe "items"
        [ test "returns the items in the column" <|
            \() ->
                Column.init True "a name" [ "item 1", "item 2" ]
                    |> Column.items
                    |> Expect.equal [ "item 1", "item 2" ]
        ]


hasName : Test
hasName =
    describe "hasName"
        [ test "returns True if the colum name matches" <|
            \() ->
                Column.init True "a name" []
                    |> Column.hasName "a name"
                    |> Expect.equal True
        , test "returns False if the colum name does not match" <|
            \() ->
                Column.init True "a name" []
                    |> Column.hasName "A NAME"
                    |> Expect.equal False
        ]
