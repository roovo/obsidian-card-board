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
                FilterHelpers.fileFilter "a/b/c.ext"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "a/b/c.ext")
                    |> Expect.equal True
        , test "returns False for a non-matching file filter" <|
            \() ->
                FilterHelpers.fileFilter "a/b/c.diff"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "a/b/c.ext")
                    |> Expect.equal False
        , test "returns True for a matching windows path filter for the full path" <|
            \() ->
                FilterHelpers.pathFilter "aa\\\\bb"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal True
        , test "returns True for a matching windows path filter with a trailing \\" <|
            \() ->
                FilterHelpers.pathFilter "aa\\\\bb\\\\"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal True
        , test "returns True for a matching windows path filter for the first part of the path" <|
            \() ->
                FilterHelpers.pathFilter "aa"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal True
        , test "returns True for a matching windows path filter of \\" <|
            \() ->
                FilterHelpers.pathFilter "\\\\"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal True
        , test "returns True for an empty windows path filter" <|
            \() ->
                FilterHelpers.pathFilter ""
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal True
        , test "returns False if the windows path filter only contains a part of the last path componant" <|
            \() ->
                FilterHelpers.pathFilter "aa\\\\b"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal False
        , test "returns False if the windows path filter contains the file name" <|
            \() ->
                FilterHelpers.pathFilter "aa\\\\bb\\\\c"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal False
        , test "returns False if the windows path filter contains the file name & extension" <|
            \() ->
                FilterHelpers.pathFilter "aa\\\\bb\\\\c.ext"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa\\bb\\c.ext")
                    |> Expect.equal False
        , test "returns True for a matching path filter for the full path" <|
            \() ->
                FilterHelpers.pathFilter "aa/bb"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal True
        , test "returns True for a matching path filter with a trailing /" <|
            \() ->
                FilterHelpers.pathFilter "aa/bb/"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal True
        , test "returns True for a matching path filter for the first part of the path" <|
            \() ->
                FilterHelpers.pathFilter "aa"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal True
        , test "returns True for a matching path filter of /" <|
            \() ->
                FilterHelpers.pathFilter "/"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal True
        , test "returns True for an empty path filter" <|
            \() ->
                FilterHelpers.pathFilter ""
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal True
        , test "returns False if the path filter only contains a part of the last path componant" <|
            \() ->
                FilterHelpers.pathFilter "aa/b"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal False
        , test "returns False if the path filter contains the file name" <|
            \() ->
                FilterHelpers.pathFilter "aa/bb/c"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal False
        , test "returns False if the path filter contains the file name & extension" <|
            \() ->
                FilterHelpers.pathFilter "aa/bb/c.ext"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo" "aa/bb/c.ext")
                    |> Expect.equal False
        , test "returns True for a matching tag filter" <|
            \() ->
                FilterHelpers.tagFilter "taga"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo #taga #tagb" "")
                    |> Expect.equal True
        , test "returns False for a non-matching tag filter" <|
            \() ->
                FilterHelpers.tagFilter "taga"
                    |> Filter.isAllowed (TaskItemHelpers.exampleTaskItem "- [ ] foo #tagb tagc" "")
                    |> Expect.equal False
        ]


ofType : Test
ofType =
    describe "ofType"
        [ test "extracts FileFilters" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> Filter.ofType "fileFilter"
                    |> Expect.equal [ FilterHelpers.fileFilter "a/file.md" ]
        , test "extracts PathFilters" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> Filter.ofType "pathFilter"
                    |> Expect.equal [ FilterHelpers.pathFilter "a/path" ]
        , test "extracts TagFilters" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> Filter.ofType "tagFilter"
                    |> Expect.equal [ FilterHelpers.tagFilter "a_tag" ]
        ]


filterType : Test
filterType =
    describe "filterType"
        [ test "extracts the filterType from a FileFilter" <|
            \() ->
                FilterHelpers.fileFilter "some file"
                    |> Filter.filterType
                    |> Expect.equal "Files"
        , test "extracts the filterType from a PathFilter" <|
            \() ->
                FilterHelpers.pathFilter "some path"
                    |> Filter.filterType
                    |> Expect.equal "Paths"
        , test "extracts the filterType from a TagFilter" <|
            \() ->
                FilterHelpers.tagFilter "some tag"
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
                FilterHelpers.fileFilter "some file"
                    |> Filter.value
                    |> Expect.equal "some file"
        , test "extracts the value from a PathFilter" <|
            \() ->
                FilterHelpers.pathFilter "some path"
                    |> Filter.value
                    |> Expect.equal "some path"
        , test "extracts the value from a TagFilter" <|
            \() ->
                FilterHelpers.tagFilter "some tag"
                    |> Filter.value
                    |> Expect.equal "some tag"
        ]
