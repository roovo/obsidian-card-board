module Form.SafeDecoder exposing
    ( Decoder
    , Validator
    , always
    , andThen
    , assert
    , custom
    , identity
    , int
    , lift
    , listOf
    , map
    , map10
    , map2
    , map7
    , minBound
    , minLength
    , run
    )

import Form.Decoder as FD



-- TYPES


type Decoder input a
    = Decoder (input -> Result Never a)


type alias Validator input =
    Decoder input input



-- DECODE FUNCTIONS


run : Decoder input a -> input -> Result Never a
run (Decoder f) a =
    f a



-- PRIMITIVE DECODERS


always : a -> Decoder input a
always =
    top


identity : Decoder input input
identity =
    custom Ok


int : Int -> Decoder String Int
int default =
    custom <| Ok << Maybe.withDefault default << String.toInt



-- PRIMITIVE VALIDATORS


minBound : comparable -> comparable -> Validator comparable
minBound default bound =
    custom <|
        \n ->
            if n >= bound then
                Ok n

            else
                Ok default


minLength : String -> Int -> Validator String
minLength default bound =
    custom <|
        \str ->
            if String.length str >= bound then
                Ok str

            else
                Ok default



-- CUSTOM DECODERS


custom : (input -> Result Never a) -> Decoder input a
custom =
    Decoder



-- HELPER FUNCTIONS FOR VALIDATION


assert : Validator a -> Decoder input a -> Decoder input a
assert v (Decoder f) =
    custom <|
        \a ->
            Result.andThen
                (\x -> run v x)
                (f a)



-- HELPERS FOR FORMS


lift : (j -> i) -> Decoder i a -> Decoder j a
lift f (Decoder g) =
    custom <| g << f


map : (a -> b) -> Decoder input a -> Decoder input b
map f (Decoder g) =
    custom <| Result.map f << g


map2 : (a -> b -> value) -> Decoder input a -> Decoder input b -> Decoder input value
map2 f d1 d2 =
    top f
        |> field d1
        |> field d2


map7 :
    (a -> b -> c -> d -> e -> f -> g -> value)
    -> Decoder input a
    -> Decoder input b
    -> Decoder input c
    -> Decoder input d
    -> Decoder input e
    -> Decoder input f
    -> Decoder input g
    -> Decoder input value
map7 f d1 d2 d3 d4 d5 d6 d7 =
    top f
        |> field d1
        |> field d2
        |> field d3
        |> field d4
        |> field d5
        |> field d6
        |> field d7


map10 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> value)
    -> Decoder input a
    -> Decoder input b
    -> Decoder input c
    -> Decoder input d
    -> Decoder input e
    -> Decoder input f
    -> Decoder input g
    -> Decoder input h
    -> Decoder input i
    -> Decoder input j
    -> Decoder input value
map10 f d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 =
    top f
        |> field d1
        |> field d2
        |> field d3
        |> field d4
        |> field d5
        |> field d6
        |> field d7
        |> field d8
        |> field d9
        |> field d10



-- ADVANCED


andThen : (a -> Decoder input b) -> Decoder input a -> Decoder input b
andThen f (Decoder g) =
    custom <|
        \a ->
            case g a of
                Err err ->
                    Err err

                Ok x ->
                    run (f x) a


listOf : Decoder a b -> Decoder (List a) (List b)
listOf d =
    custom <|
        \ls ->
            List.foldr appendListResult (Ok []) <|
                List.indexedMap (\n -> runWithTag n d) ls



-- PRIVATE


appendListResult : Result Never b -> Result Never (List b) -> Result Never (List b)
appendListResult r1 r2 =
    case ( r1, r2 ) of
        ( Err err, Err _ ) ->
            Err err

        ( Err err, Ok _ ) ->
            Err err

        ( Ok _, Err errs ) ->
            Err errs

        ( Ok b, Ok bs ) ->
            Ok (b :: bs)


field : Decoder i a -> Decoder i (a -> b) -> Decoder i b
field (Decoder f) (Decoder g) =
    custom <|
        \i ->
            case ( g i, f i ) of
                ( Ok h, res ) ->
                    Result.map h res

                ( Err gErr, Err _ ) ->
                    Err gErr

                ( Err gErr, Ok _ ) ->
                    Err gErr


runWithTag : tag -> Decoder a b -> a -> Result Never b
runWithTag tag d a =
    run d a


top : f -> Decoder i f
top f =
    custom <| \_ -> Ok f
