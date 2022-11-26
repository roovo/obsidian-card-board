module Helpers.UnicodeHelpers exposing
    ( nonWhitespaceFuzzer
    , whitespaceFuzzer
    )

import Fuzz exposing (Fuzzer)


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



-- PRIVATE


whitespaceCodes : List Int
whitespaceCodes =
    [ 0x20, 0xA0, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008, 0x2009, 0x200A, 0x2028, 0x205F, 0x3000 ]
