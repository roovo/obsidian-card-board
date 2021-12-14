module FilterTests exposing (suite)

import Expect
import Filter exposing (Filter)
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import Test exposing (..)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ encodeDecode
        , isAllowed
        , ofType
        , filterType
        , filterTypes
        , value
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding filters"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> TsEncode.runExample (TsEncode.list Filter.encoder)
                    |> .output
                    |> DecodeHelpers.runDecoder (TsDecode.list Filter.decoder)
                    |> .decoded
                    |> Expect.equal (Ok FilterHelpers.exampleFilters)
        ]


isAllowed : Test
isAllowed =
    describe "isAllowed"
        [ test "returns True for a matching file filter" <|
            \() ->
                Filter.FileFilter "a/b/c.ext"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "a/b/c.ext")
                    |> Expect.equal True
        , test "returns False for a non-matching file filter" <|
            \() ->
                Filter.FileFilter "a/b/c.diff"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "a/b/c.ext")
                    |> Expect.equal False
        ]


ofType : Test
ofType =
    describe "ofType"
        [ test "extracts FileFilters" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> Filter.ofType "fileFilter"
                    |> Expect.equal [ Filter.FileFilter "a/file.md" ]
        , test "extracts PathFilters" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> Filter.ofType "pathFilter"
                    |> Expect.equal [ Filter.PathFilter "a/path" ]
        , test "extracts TagFilters" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> Filter.ofType "tagFilter"
                    |> Expect.equal [ Filter.TagFilter "a_tag" ]
        ]


filterType : Test
filterType =
    describe "filterType"
        [ test "extracts the filterType from a FileFilter" <|
            \() ->
                Filter.FileFilter "some file"
                    |> Filter.filterType
                    |> Expect.equal "Files"
        , test "extracts the filterType from a PathFilter" <|
            \() ->
                Filter.PathFilter "some path"
                    |> Filter.filterType
                    |> Expect.equal "Paths"
        , test "extracts the filterType from a TagFilter" <|
            \() ->
                Filter.TagFilter "some tag"
                    |> Filter.filterType
                    |> Expect.equal "Tags"
        ]


filterTypes : Test
filterTypes =
    describe "filterTypes"
        [ test "return the filter types" <|
            \() ->
                Filter.filterTypes
                    |> Expect.equal [ "Files", "Paths", "Tags" ]
        ]


value : Test
value =
    describe "value"
        [ test "extracts the value from a FileFilter" <|
            \() ->
                Filter.FileFilter "some file"
                    |> Filter.value
                    |> Expect.equal "some file"
        , test "extracts the value from a PathFilter" <|
            \() ->
                Filter.PathFilter "some path"
                    |> Filter.value
                    |> Expect.equal "some path"
        , test "extracts the value from a TagFilter" <|
            \() ->
                Filter.TagFilter "some tag"
                    |> Filter.value
                    |> Expect.equal "some tag"
        ]
