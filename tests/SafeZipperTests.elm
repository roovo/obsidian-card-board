module SafeZipperTests exposing (suite)

import Expect
import Fuzz
import SafeZipper
import Test exposing (..)


suite : Test
suite =
    concat
        [ add
        , atIndex
        , current
        , currentIndex
        , deleteCurrent
        , empty
        , findIndex
        , first
        , fromList
        , indexedMapSelectedAndRest
        , last
        , length
        , map
        , mapSelectedAndRest
        , next
        , selectedIndex
        , toList
        , updateCurrent
        ]


add : Test
add =
    describe "add"
        [ test "adds an item to an empty zipper" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.add 1
                    |> SafeZipper.toList
                    |> Expect.equal [ 1 ]
        , test "adds an item to the end of a non-empty zipper" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 1
                    |> SafeZipper.add 5
                    |> SafeZipper.toList
                    |> Expect.equal [ 1, 2, 3, 4, 5 ]
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
        , test "doesn't change the order of the items in the list" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.toList
                    |> Expect.equal [ 1, 2, 3, 4 ]
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


currentIndex : Test
currentIndex =
    describe "currentIndex"
        [ test "returns Just the index of the current list item if the list has items" <|
            \() ->
                [ 'a', 'b', 'c' ]
                    |> SafeZipper.fromList
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 0)
        , test "returns Nothing if the list has NO items" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.currentIndex
                    |> Expect.equal Nothing
        ]


deleteCurrent : Test
deleteCurrent =
    describe "deleteCurrent"
        [ test "returns an empty zipper if given one" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.deleteCurrent
                    |> SafeZipper.toList
                    |> Expect.equal []
        , test "returns an empty zipper if given one containing only a single element" <|
            \() ->
                [ 1 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.deleteCurrent
                    |> SafeZipper.toList
                    |> Expect.equal []
        , test "returns an zipper focussed on the first element if the first element is deleted" <|
            \() ->
                [ 1, 2, 3 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.deleteCurrent
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 0)
        , test "returns an zipper focussed on the last element if the last element is deleted" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 3
                    |> SafeZipper.deleteCurrent
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 2)
        , test "doesn't mess with the list ordering" <|
            \() ->
                [ 1, 2, 3, 4, 5, 6 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.atIndex 3
                    |> SafeZipper.deleteCurrent
                    |> SafeZipper.toList
                    |> Expect.equal [ 1, 2, 3, 5, 6 ]
        , test "returns an zipper focussed on the element at the same index otherwise" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 1
                    |> SafeZipper.deleteCurrent
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 1)
        ]


empty : Test
empty =
    describe "empty"
        [ test "returns an empty zipper" <|
            \() ->
                SafeZipper.empty
                    |> SafeZipper.toList
                    |> Expect.equal []
        ]


findIndex : Test
findIndex =
    describe "findIndex"
        [ test "returns Nothing for an empty zipper" <|
            \() ->
                SafeZipper.empty
                    |> SafeZipper.findIndex (always True)
                    |> Expect.equal Nothing
        , test "returns Nothing for a single item zipper where the first item is not found" <|
            \() ->
                [ "one" ]
                    |> SafeZipper.fromList
                    |> SafeZipper.findIndex (always False)
                    |> Expect.equal Nothing
        , test "returns Just 0 for a single item zipper where the first item is found" <|
            \() ->
                [ "one" ]
                    |> SafeZipper.fromList
                    |> SafeZipper.findIndex (always True)
                    |> Expect.equal (Just 0)
        , test "returns Just the index for a multiple item zipper where there is a match" <|
            \() ->
                [ "one", "two", "three", "four", "five" ]
                    |> SafeZipper.fromList
                    |> SafeZipper.findIndex (\s -> String.length s == 4)
                    |> Expect.equal (Just 3)
        ]


first : Test
first =
    describe "first"
        [ test "returns an empty zipper if given one" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.first
                    |> Expect.equal SafeZipper.empty
        , test "returns an zipper focussed on the first item" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.first
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 0)
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
        [ test "returns an empty zipper if given one" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.indexedMapSelectedAndRest (\_ x -> ( 9, x )) Tuple.pair
                    |> Expect.equal SafeZipper.empty
        , test "returns a zipper focussed on a mid-list item" <|
            \() ->
                [ 5, 4, 3, 2, 1 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.indexedMapSelectedAndRest (\_ x -> ( 9, x )) Tuple.pair
                    |> SafeZipper.toList
                    |> Expect.equal [ ( 0, 5 ), ( 1, 4 ), ( 9, 3 ), ( 3, 2 ), ( 4, 1 ) ]
        ]


last : Test
last =
    describe "last"
        [ test "focusses on Nothing for an empty zipper" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.last
                    |> SafeZipper.currentIndex
                    |> Expect.equal Nothing
        , test "focusses on the last item for a NON empty zipper" <|
            \() ->
                [ 1, 2, 3 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.last
                    |> SafeZipper.currentIndex
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


map : Test
map =
    describe "map"
        [ test "applies the map function to all the zipper items" <|
            \() ->
                SafeZipper.fromList [ 1, 2, 3 ]
                    |> SafeZipper.atIndex 1
                    |> SafeZipper.map ((+) 10)
                    |> SafeZipper.toList
                    |> Expect.equal [ 11, 12, 13 ]
        ]


mapSelectedAndRest : Test
mapSelectedAndRest =
    describe "mapSelectedAndRest"
        [ test "returns an empty zipper if given one" <|
            \() ->
                []
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.mapSelectedAndRest (\_ x -> ( 9, x )) Tuple.pair
                    |> Expect.equal SafeZipper.empty
        , test "returns a zipper focussed on a mid-list item" <|
            \() ->
                [ 5, 4, 3, 2, 1 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.atIndex 2
                    |> SafeZipper.mapSelectedAndRest (always 9) (\x -> x)
                    |> SafeZipper.toList
                    |> Expect.equal [ 5, 4, 9, 2, 1 ]
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
        , test "doesn't reorder the items in the zippeer" <|
            \() ->
                [ 1, 2, 3, 4 ]
                    |> SafeZipper.fromList
                    |> SafeZipper.next
                    |> SafeZipper.next
                    |> SafeZipper.toList
                    |> Expect.equal [ 1, 2, 3, 4 ]
        ]


selectedIndex : Test
selectedIndex =
    describe "selectedIndex"
        [ test "is Nothing for an empty zipper" <|
            \() ->
                SafeZipper.empty
                    |> SafeZipper.selectedIndex
                    |> Expect.equal Nothing
        , test "is Just 0 for a newly built zipper with stuff in it" <|
            \() ->
                SafeZipper.fromList [ 1, 2 ]
                    |> SafeZipper.selectedIndex
                    |> Expect.equal (Just 0)
        , test "is Just 1 for a zipper pointing at the second item" <|
            \() ->
                SafeZipper.fromList [ 1, 2, 3, 4 ]
                    |> SafeZipper.next
                    |> SafeZipper.selectedIndex
                    |> Expect.equal (Just 1)
        ]


toList : Test
toList =
    describe "toList"
        [ test "returns an empty list for an empty zipper" <|
            \() ->
                SafeZipper.empty
                    |> SafeZipper.toList
                    |> Expect.equal []
        , test "returns the zipper contents as a list" <|
            \() ->
                SafeZipper.fromList [ 1, 2, 3, 4, 5, 6 ]
                    |> SafeZipper.atIndex 3
                    |> SafeZipper.toList
                    |> Expect.equal [ 1, 2, 3, 4, 5, 6 ]
        ]


updateCurrent : Test
updateCurrent =
    describe "updateCurrent"
        [ test "returns an empty list if given one" <|
            \() ->
                SafeZipper.fromList []
                    |> SafeZipper.atIndex 1
                    |> SafeZipper.updateCurrent ((+) 10)
                    |> Expect.equal SafeZipper.empty
        , test "applies the map function to the current item only" <|
            \() ->
                SafeZipper.fromList [ 1, 2, 3 ]
                    |> SafeZipper.atIndex 1
                    |> SafeZipper.updateCurrent ((+) 10)
                    |> SafeZipper.toList
                    |> Expect.equal [ 1, 12, 3 ]
        ]
