module SafeZipper exposing
    ( SafeZipper
    , atIndex
    , current
    , fromList
    , indexedMapSelectedAndRest
    , length
    , next
    , toList
    )

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



-- ACCESSORS


current : SafeZipper a -> Maybe a
current zipper =
    case zipper of
        EmptyZipper ->
            Nothing

        SafeZipper b c a ->
            Just c


toList : SafeZipper a -> List a
toList zipper =
    case zipper of
        EmptyZipper ->
            []

        SafeZipper ls x rs ->
            List.append ls (x :: rs)



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

        SafeZipper b c a ->
            let
                beforeLength =
                    List.length b

                mappedBefore =
                    List.indexedMap restFn b

                mappedCurrent =
                    selectedFn beforeLength c

                mappedAfter =
                    List.indexedMap (\i item -> restFn (beforeLength + 1 + i) item) a
            in
            SafeZipper mappedBefore mappedCurrent mappedAfter



-- UTILITIES


length : SafeZipper a -> Int
length zipper =
    case zipper of
        EmptyZipper ->
            0

        SafeZipper b c a ->
            List.length b + 1 + List.length a
