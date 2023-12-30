module Form.SafeDecoderTests exposing (suite)

import Expect
import Form.SafeDecoder as SafeDecoder
import Test exposing (..)


suite : Test
suite =
    concat
        [ always
        , andThen
        , assert
        , identity
        , int
        , lift
        , listOf
        , map
        , map2
        , minBound
        , minLength
        ]


always : Test
always =
    describe "always"
        [ test "always succeeds with the given value" <|
            \() ->
                SafeDecoder.run (SafeDecoder.always "foo") "bar"
                    |> Expect.equal (Ok "foo")
        ]


andThen : Test
andThen =
    describe "andThen"
        [ test "chains two decoders" <|
            \() ->
                let
                    decoder : SafeDecoder.Decoder String Int
                    decoder =
                        SafeDecoder.andThen
                            (\str ->
                                if String.startsWith "foo" str then
                                    SafeDecoder.int 7

                                else
                                    SafeDecoder.int 0
                            )
                            SafeDecoder.identity
                in
                SafeDecoder.run decoder "foooey"
                    |> Expect.equal (Ok 7)
        ]


assert : Test
assert =
    describe "assert"
        [ test "returns the input if it is valid for the given validator" <|
            \() ->
                SafeDecoder.int 3
                    |> SafeDecoder.assert (SafeDecoder.minBound 10 0)
                    |> (\d -> SafeDecoder.run d "7")
                    |> Expect.equal (Ok 7)
        , test "returns the default if it is NOT valid for the given validator" <|
            \() ->
                SafeDecoder.int 3
                    |> SafeDecoder.assert (SafeDecoder.minBound 10 0)
                    |> (\d -> SafeDecoder.run d "-7")
                    |> Expect.equal (Ok 10)
        ]


identity : Test
identity =
    describe "identity"
        [ test "decodes a string" <|
            \() ->
                SafeDecoder.run SafeDecoder.identity "foo"
                    |> Expect.equal (Ok "foo")
        ]


int : Test
int =
    describe "int"
        [ test "decodes a positive input" <|
            \() ->
                SafeDecoder.run (SafeDecoder.int 3) "7"
                    |> Expect.equal (Ok 7)
        , test "decodes a negative input" <|
            \() ->
                SafeDecoder.run (SafeDecoder.int 3) "-7"
                    |> Expect.equal (Ok -7)
        , test "sets an empty input to the default" <|
            \() ->
                SafeDecoder.run (SafeDecoder.int 3) ""
                    |> Expect.equal (Ok 3)
        , test "sets a whitespace input to the default" <|
            \() ->
                SafeDecoder.run (SafeDecoder.int 3) "   "
                    |> Expect.equal (Ok 3)
        , test "sets an invalid input to the default" <|
            \() ->
                SafeDecoder.run (SafeDecoder.int 3) "x"
                    |> Expect.equal (Ok 3)
        , test "sets a decimal input to the default" <|
            \() ->
                SafeDecoder.run (SafeDecoder.int 3) "1.2"
                    |> Expect.equal (Ok 3)
        ]


lift : Test
lift =
    describe "lift"
        [ test "lifts a function into the decoder" <|
            \() ->
                SafeDecoder.run (SafeDecoder.lift .field1 <| SafeDecoder.minLength "eek" 3) (Form "food" "2")
                    |> Expect.equal (Ok "food")
        ]


listOf : Test
listOf =
    describe "listOf"
        [ test "lifts a function into the decoder" <|
            \() ->
                SafeDecoder.run (SafeDecoder.listOf <| SafeDecoder.int 3) [ "7", "poo", "0", "1" ]
                    |> Expect.equal (Ok [ 7, 3, 0, 1 ])
        ]


map : Test
map =
    describe "map"
        [ test "transform the result of a decoder" <|
            \() ->
                SafeDecoder.run (SafeDecoder.map String.length SafeDecoder.identity) "food"
                    |> Expect.equal (Ok <| 4)
        ]


map2 : Test
map2 =
    describe "map2"
        [ test "combines the result of two decoders" <|
            \() ->
                let
                    formDecoder : SafeDecoder.Decoder Form Decoded
                    formDecoder =
                        SafeDecoder.map2 Decoded
                            (SafeDecoder.lift .field1 SafeDecoder.identity)
                            (SafeDecoder.lift .field2 (SafeDecoder.int 10))
                in
                SafeDecoder.run formDecoder (Form "foo" "8")
                    |> Expect.equal (Ok <| Decoded "foo" 8)
        ]


minBound : Test
minBound =
    describe "minBound"
        [ test "returns the default if outside the bounds" <|
            \() ->
                SafeDecoder.run (SafeDecoder.minBound 3 10) 2
                    |> Expect.equal (Ok 3)
        , test "returns the input if inside the bounds" <|
            \() ->
                SafeDecoder.run (SafeDecoder.minBound 3 10) 20
                    |> Expect.equal (Ok 20)
        ]


minLength : Test
minLength =
    describe "minLength"
        [ test "returns the default if outside the bounds" <|
            \() ->
                SafeDecoder.run (SafeDecoder.minLength "foo" 3) "a"
                    |> Expect.equal (Ok "foo")
        , test "returns the input if inside the bounds" <|
            \() ->
                SafeDecoder.run (SafeDecoder.minLength "foo" 3) "aaa"
                    |> Expect.equal (Ok "aaa")
        ]



-- HELPERS


type alias Decoded =
    { str : String
    , int : Int
    }


type alias Form =
    { field1 : String
    , field2 : String
    }
