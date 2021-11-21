module MarkdownFileTests exposing (suite)

import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import MarkdownFile exposing (decoder)
import Test exposing (..)
import TsJson.Decode as TsDecode


suite : Test
suite =
    concat
        [ decoderTests
        ]


decoderTests : Test
decoderTests =
    describe "decoder"
        [ test "decodes a MarkdownFile with a fileDate" <|
            \() ->
                """{"filePath":"a path","fileDate":"a date","fileContents":"some contents"}"""
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", fileContents = "some contents" })
        , test "decodes a MarkdownFile with a null date" <|
            \() ->
                """{"filePath":"a path","fileDate":null,"fileContents":"some contents"}"""
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Nothing, fileContents = "some contents" })
        , test "fails if a the fileDate field is missing" <|
            \() ->
                """{"filePath":"a path","fileContents":"some contents"}"""
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "builds the correct tsType" <|
            \() ->
                ""
                    |> DecodeHelpers.runDecoder decoder
                    |> .tsType
                    |> Expect.equal "{ fileContents : string; fileDate : string | null; filePath : string }"
        ]
