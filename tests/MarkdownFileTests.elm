module MarkdownFileTests exposing (suite)

import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import MarkdownFile exposing (decoder)
import Set
import Test exposing (..)


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
                    |> Result.map .fileDate
                    |> Expect.equal (Ok (Just "a date"))
        , test "decodes a MarkdownFile with a null date" <|
            \() ->
                """{"filePath":"a path","fileDate":null,"fileContents":"some contents"}"""
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .fileDate
                    |> Expect.equal (Ok Nothing)
        , test "fails if a the fileDate field is missing" <|
            \() ->
                """{"filePath":"a path","fileContents":"some contents"}"""
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "does not include an empty front matter block in the body" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .body
                    |> Expect.equal (Ok "some contents")
        , test "extracts (inline) tags from the front matter block" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .frontMatterTags
                    |> Expect.equal (Ok <| Set.fromList [ "tag1", "tag2" ])
        , test "extracts (top level) tags from the front matter block" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags:\\n- tag1\\n- tag2\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .frontMatterTags
                    |> Expect.equal (Ok <| Set.fromList [ "tag1", "tag2" ])
        , test "does not include the front matter block in the body" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags:\\n- tag1\\n- tag2\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .body
                    |> Expect.equal (Ok "some contents")
        , test "ignores other items in the front matter block" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\nfoo: bar\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .frontMatterTags
                    |> Expect.equal (Ok <| Set.fromList [ "tag1", "tag2" ])
        , test "extracts (top level) tags from the front matter block if preceeded by whitespace" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"  \\n\\n---\\ntags:\\n- tag1\\n- tag2\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .frontMatterTags
                    |> Expect.equal (Ok <| Set.fromList [ "tag1", "tag2" ])
        , test "does not extract front matter if it is not at the start of the file" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"x\\n---\\ntags: [ tag1, tag2 ]\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .frontMatterTags
                    |> Expect.equal (Ok Set.empty)
        , test "includes the front matter block in the body if it is not at the start of the file" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"x\\n---\\ntags: [ tag1, tag2 ]\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .body
                    |> Expect.equal (Ok "x\n---\ntags: [ tag1, tag2 ]\n---\nsome contents")
        , test "does not extact front matter if it contains invalid yaml" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\n$ is not valid\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .frontMatterTags
                    |> Expect.equal (Ok Set.empty)
        , test "does not include the the front matter block in the body if it is invalid yaml" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\n$ is not valid\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .body
                    |> Expect.equal (Ok "some contents")
        , test "sets the body to an empty string if there is only front matter" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\n\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .body
                    |> Expect.equal (Ok "")
        , test "does not extract the front matter if there is only front matter" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2 ]\\n\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .frontMatterTags
                    |> Expect.equal (Ok Set.empty)
        , test "sets the bodyOffest to 0 if there is no front matter" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"some contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .bodyOffset
                    |> Expect.equal (Ok 0)
        , test "sets the bodyOffest to 0 if there is only front matter" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .bodyOffset
                    |> Expect.equal (Ok 0)
        , test "sets the bodyOffest to the number of lines for the front matter if it is present" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2]\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .bodyOffset
                    |> Expect.equal (Ok 3)
        , test "bodyOffest takes in to account any whitespace lines before the front matter" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"   \\n  \\n\\n---\\ntags: [ tag1, tag2]\\n---\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .bodyOffset
                    |> Expect.equal (Ok 6)
        , test "body includes any whitespace lines after the front matter" <|
            \() ->
                "{\"filePath\":\"a path\",\"fileDate\":\"a date\",\"fileContents\":\"---\\ntags: [ tag1, tag2]\\n---\\n\\nsome contents\"}"
                    |> DecodeHelpers.runDecoder decoder
                    |> .decoded
                    |> Result.map .body
                    |> Expect.equal (Ok "\nsome contents")
        , test "builds the correct tsType" <|
            \() ->
                ""
                    |> DecodeHelpers.runDecoder decoder
                    |> .tsType
                    |> Expect.equal "{ fileContents : string; fileDate : string | null; filePath : string }"
        ]
