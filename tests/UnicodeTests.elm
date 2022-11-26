module UnicodeTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
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
        [ fuzz whitespaceFuzzer "returns True for all unicode whitespace" <|
            \fuzzedChar ->
                fuzzedChar
                    |> Unicode.isWhitespace
                    |> Expect.equal True
        , fuzz nonWhitespaceFuzzer "returns False for all unicode NON whitespace characters" <|
            \fuzzedChar ->
                fuzzedChar
                    |> Unicode.isWhitespace
                    |> Expect.equal False
        ]


nonWhitespaceFuzzer : Fuzzer Char
nonWhitespaceFuzzer =
    let
        replaceIfWhitespace : Char -> Char
        replaceIfWhitespace c =
            c
                |> Char.toCode
                |> (\code ->
                        if List.member code whitespaceCodes then
                            'a'

                        else
                            Char.fromCode code
                   )
    in
    Fuzz.char
        |> Fuzz.map replaceIfWhitespace


whitespaceFuzzer : Fuzzer Char
whitespaceFuzzer =
    Fuzz.oneOfValues whitespaceCodes
        |> Fuzz.map Char.fromCode


whitespaceCodes : List Int
whitespaceCodes =
    [ 0x20, 0xA0, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200A, 0x2028, 0x205F, 0x3000 ]
