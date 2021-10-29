module SafeZipper exposing
    ( SafeZipper
    , add
    , atIndex
    , current
    , currentIndex
    , deleteCurrent
    , fromList
    , indexedMapSelectedAndRest
    , last
    , length
    , mapCurrent
    , next
    , selectedIndex
    , toList
    )

import List.Extra as LE



-- TYPES


type SafeZipper a
    = EmptyZipper
    | SafeZipper (List a) a (List a)



-- CONSTRUCTION


fromList : List a -> SafeZipper a
fromList xs =
    case xs of
        [] ->
            EmptyZipper

        y :: ys ->
            SafeZipper [] y ys


add : a -> SafeZipper a -> SafeZipper a
add element zipper =
    case zipper of
        EmptyZipper ->
            fromList [ element ]

        SafeZipper b c a ->
            SafeZipper b c (a ++ [ element ])



-- ACCESSORS


current : SafeZipper a -> Maybe a
current zipper =
    case zipper of
        EmptyZipper ->
            Nothing

        SafeZipper b c a ->
            Just c


currentIndex : SafeZipper a -> Maybe Int
currentIndex zipper =
    case zipper of
        EmptyZipper ->
            Nothing

        SafeZipper ls x rs ->
            Just <| List.length ls


toList : SafeZipper a -> List a
toList zipper =
    case zipper of
        EmptyZipper ->
            []

        SafeZipper ls x rs ->
            List.reverse ls ++ [ x ] ++ rs



-- MOVING AROUND


atIndex : Int -> SafeZipper a -> SafeZipper a
atIndex index zipper =
    case zipper of
        EmptyZipper ->
            zipper

        SafeZipper ls x rs ->
            let
                moveAlong : Int -> SafeZipper a -> SafeZipper a
                moveAlong _ z =
                    next z
            in
            List.foldl moveAlong (first zipper) (List.repeat index 0)


first : SafeZipper a -> SafeZipper a
first zipper =
    case zipper of
        EmptyZipper ->
            zipper

        SafeZipper ls x rs ->
            case List.reverse ls of
                [] ->
                    zipper

                y :: ys ->
                    SafeZipper [] y (ys ++ [ x ] ++ rs)


last : SafeZipper a -> SafeZipper a
last zipper =
    case zipper of
        EmptyZipper ->
            zipper

        SafeZipper ls x rs ->
            case List.reverse rs of
                [] ->
                    zipper

                y :: ys ->
                    SafeZipper (ys ++ [ x ] ++ ls) y []


next : SafeZipper a -> SafeZipper a
next zipper =
    case zipper of
        EmptyZipper ->
            zipper

        SafeZipper ls x rs ->
            case rs of
                [] ->
                    zipper

                y :: ys ->
                    SafeZipper (x :: ls) y ys



-- MAPPING


indexedMapSelectedAndRest : (Int -> a -> b) -> (Int -> a -> b) -> SafeZipper a -> SafeZipper b
indexedMapSelectedAndRest selectedFn restFn zipper =
    case zipper of
        EmptyZipper ->
            EmptyZipper

        SafeZipper ls c rs ->
            let
                beforeLength =
                    List.length ls

                mappedBefore =
                    ls
                        |> List.reverse
                        |> List.indexedMap restFn
                        |> List.reverse

                mappedCurrent =
                    selectedFn beforeLength c

                mappedAfter =
                    List.indexedMap (\i item -> restFn (beforeLength + 1 + i) item) rs
            in
            SafeZipper mappedBefore mappedCurrent mappedAfter


mapCurrent : (a -> a) -> SafeZipper a -> SafeZipper a
mapCurrent fn zipper =
    case zipper of
        EmptyZipper ->
            zipper

        SafeZipper b c a ->
            SafeZipper b (fn c) a



-- UTILITIES


deleteCurrent : SafeZipper a -> SafeZipper a
deleteCurrent zipper =
    let
        index =
            zipper
                |> currentIndex
                |> Maybe.withDefault 0
    in
    zipper
        |> toList
        |> LE.removeAt index
        |> fromList
        |> atIndex index


length : SafeZipper a -> Int
length zipper =
    case zipper of
        EmptyZipper ->
            0

        SafeZipper b c a ->
            List.length b + 1 + List.length a


selectedIndex : SafeZipper a -> Maybe Int
selectedIndex zipper =
    case zipper of
        EmptyZipper ->
            Nothing

        SafeZipper b _ _ ->
            Just <| List.length b
