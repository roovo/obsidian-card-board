module UnicodeTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Helpers.UnicodeHelpers as UnicodeHelpers
import Test exposing (..)
import Unicode


suite : Test
suite =
    concat
        [ codes
        , isWhitespace
        ]


codes : Test
codes =
    describe "codes"
        [ test "basicLatinEndCode is U+007F" <|
            \() ->
                Unicode.basicLatinEndCode
                    |> Expect.equal 0x7F
        , test "forwardslashCode is the code for '/'" <|
            \() ->
                Unicode.forwardslashCode
                    |> Char.fromCode
                    |> Expect.equal '/'
        , test "minusCode is the code for '-'" <|
            \() ->
                Unicode.minusCode
                    |> Char.fromCode
                    |> Expect.equal '-'
        , test "underscoreCode is the code for '_'" <|
            \() ->
                Unicode.underscoreCode
                    |> Char.fromCode
                    |> Expect.equal '_'
        ]


isWhitespace : Test
isWhitespace =
    describe "isWhitespace"
        [ fuzz UnicodeHelpers.whitespaceFuzzer "returns True for all unicode whitespace" <|
            \fuzzedChar ->
                fuzzedChar
                    |> Unicode.isWhitespace
                    |> Expect.equal True
        , fuzz UnicodeHelpers.nonWhitespaceFuzzer "returns False for all unicode NON whitespace characters" <|
            \fuzzedChar ->
                fuzzedChar
                    |> Unicode.isWhitespace
                    |> Expect.equal False
        ]
