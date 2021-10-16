module SafeZipperTests exposing (suite)

import Expect
import Fuzz
import SafeZipper
import Test exposing (..)


suite : Test
suite =
    concat
        [ atIndex
        , current
        , fromList
        , indexedMapSelectedAndRest
        , length
        , next
        ]


atIndex : Test
atIndex =
    describe "atIndex"
        [ test "returns a zipper focussed on a mid-list item" <|
            \() ->
                [ 1, 2, 3, 4, 5 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 3
                    |> SafeZipper.current
                    |> Expect.equal (Just 4)
        , test "returns a zipper focussed on the first item" <|
            \() ->
                [ 1, 2, 3, 4, 5 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 0
                    |> SafeZipper.current
                    |> Expect.equal (Just 1)
        , test "returns a zipper focussed on the last item" <|
            \() ->
                [ 1, 2, 3, 4, 5 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 4
                    |> SafeZipper.current
                    |> Expect.equal (Just 5)
        , test "returns a zipper focussed on the last item if the index is past the end of the zipper" <|
            \() ->
                [ 1, 2, 3, 4, 5 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 9
                    |> SafeZipper.current
                    |> Expect.equal (Just 5)
        , test "returns a zipper focussed on the first item if the index is negative" <|
            \() ->
                [ 1, 2, 3, 4, 5 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex -1
                    |> SafeZipper.current
                    |> Expect.equal (Just 1)
        , test "returns an empty zipper if given one" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex -1
                    |> SafeZipper.current
                    |> Expect.equal Nothing
        ]


current : Test
current =
    describe "current"
        [ test "returns Just the current list item if the list has items" <|
            \() ->
                [ 1, 2, 3 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.current
                    |> Expect.equal (Just 1)
        , test "returns Nothing if the list has NO items" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.current
                    |> Expect.equal Nothing
        ]


fromList : Test
fromList =
    describe "fromIndex"
        [ test "returns a zipper focussed on the first item in the list" <|
            \() ->
                [ 1, 2, 3 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.current
                    |> Expect.equal (Just 1)
        , test "returns an empty zipper if given an empty list" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.length
                    |> Expect.equal 0
        ]


indexedMapSelectedAndRest : Test
indexedMapSelectedAndRest =
    describe "indexedMapSelectedAndRest"
        [ test "returns a zipper focussed on a mid-list item" <|
            \() ->
                [ 6, 6, 6, 6, 6 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.indexedMapSelectedAndRest (\_ x -> ( 9, x )) Tuple.pair
                    |> Expect.equal [ ( 0, 6 ), ( 1, 6 ), ( 9, 6 ), ( 3, 6 ), ( 4, 6 ) ]
        ]


next : Test
next =
    describe "next"
        [ test "returns an empty zipper if given one" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.next
                    |> SafeZipper.length
                    |> Expect.equal 0
        , test "moves on to the next item in the zipper" <|
            \() ->
                [ 1, 2 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.next
                    |> SafeZipper.current
                    |> Expect.equal (Just 2)
        , test "is clamped to the end of the zipper" <|
            \() ->
                [ 1, 2 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.next
                    |> SafeZipper.next
                    |> SafeZipper.current
                    |> Expect.equal (Just 2)
        ]


length : Test
length =
    describe "length"
        [ fuzz (Fuzz.list Fuzz.string) "is correct for various sized zippers" <|
            \randomList ->
                SafeZipper.fromList randomList
                    |> SafeZipper.length
                    |> Expect.equal (List.length randomList)
        ]
