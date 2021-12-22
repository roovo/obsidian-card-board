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
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [], fileContents = "some contents" })
        , test "decodes a MarkdownFile with a null date" <|
            \() ->
                """{"filePath":"a path","fileDate":null,"fileContents":"some contents"}"""
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Nothing, frontMatterTags = [], fileContents = "some contents" })
        , test "fails if a the fileDate field is missing" <|
            \() ->
                """{"filePath":"a path","fileContents":"some contents"}"""
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "ignores an empty front matter block" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [], fileContents = "some contents" })
        , test "extracts (inline) tags from the front matter block" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [ "tag1", "tag2" ], fileContents = "some contents" })
        , test "extracts (top level) tags from the front matter block" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags:\\n- tag1\\n- tag2\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [ "tag1", "tag2" ], fileContents = "some contents" })
        , test "ignores other items in the front matter block" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\nfoo: bar\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [ "tag1", "tag2" ], fileContents = "some contents" })
        , test "extracts (top level) tags from the front matter block if preceeded by whitespace" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"  \\n\\n---\\ntags:\\n- tag1\\n- tag2\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [ "tag1", "tag2" ], fileContents = "some contents" })
        , test "ignores the front matter block if it is not at the start of the file" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"x\\n---\\ntags: [ tag1, tag2 ]\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [], fileContents = "x\n---\ntags: [ tag1, tag2 ]\n---\nsome contents" })
        , test "ignores the front matter block if it is invalid yaml" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\n$ is not valid\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Expect.equal (Ok { filePath = "a path", fileDate = Just "a date", frontMatterTags = [], fileContents = "some contents" })
        , test "builds the correct tsType" <|
            \() ->
                ""
                    |> DecodeHelpers.runDecoder decoder
                    |> .tsType
                    |> Expect.equal "{ fileContents : string; fileDate : string | null; filePath : string }"
        ]
