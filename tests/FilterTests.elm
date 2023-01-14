module FilterTests exposing (suite)

import Expect
import Filter
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Test exposing (..)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ defaultPolarity
        , defaultScope
        , encodeDecode
        , filterType
        , filterTypes
        , isAllowed
        , ofType
        , polarityFromString
        , updatePath
        , value
        ]


defaultPolarity : Test
defaultPolarity =
    describe "defaultPolarity"
        [ test "is Allow" <|
            \() ->
                Filter.defaultPolarity
                    |> Expect.equal Filter.Allow
        ]


defaultScope : Test
defaultScope =
    describe "defaultScope"
        [ test "is Both" <|
            \() ->
                Filter.defaultScope
                    |> Expect.equal Filter.Both
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding"
        [ test "can decode the encoded example filter string back to the original" <|
            \() ->
                FilterHelpers.exampleFilters
                    |> TsEncode.runExample (TsEncode.list Filter.encoder)
                    |> .output
                    |> DecodeHelpers.runDecoder (TsDecode.list Filter.decoder)
                    |> .decoded
                    |> Expect.equal (Ok FilterHelpers.exampleFilters)
        , test "can decode an encoded Filter.Allow Polarity string" <|
            \() ->
                "\"Allow\""
                    |> DecodeHelpers.runDecoder Filter.polarityDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.Allow)
        , test "can decode an encoded Filter.Deny Polarity string" <|
            \() ->
                "\"Deny\""
                    |> DecodeHelpers.runDecoder Filter.polarityDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.Deny)
        , test "can encode and decode Filter.Allow" <|
            \() ->
                Filter.Allow
                    |> TsEncode.runExample Filter.polarityEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder Filter.polarityDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.Allow)
        , test "can decode the encoded Deny Polarity string back to the original" <|
            \() ->
                Filter.Deny
                    |> TsEncode.runExample Filter.polarityEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder Filter.polarityDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.Deny)
        , test "can decode an encoded Filter.TopLevelOnly Scope string" <|
            \() ->
                "\"TopLevelOnly\""
                    |> DecodeHelpers.runDecoder Filter.scopeDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.TopLevelOnly)
        , test "can decode an encoded Filter.SubTasksOnly Scope string" <|
            \() ->
                "\"SubTasksOnly\""
                    |> DecodeHelpers.runDecoder Filter.scopeDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.SubTasksOnly)
        , test "can decode an encoded Filter.Both Scope string" <|
            \() ->
                "\"Both\""
                    |> DecodeHelpers.runDecoder Filter.scopeDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.Both)
        , test "can encode and decode Filter.TopLevelOnly" <|
            \() ->
                Filter.TopLevelOnly
                    |> TsEncode.runExample Filter.scopeEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder Filter.scopeDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.TopLevelOnly)
        , test "can encode and decode Filter.SubTasksOnly" <|
            \() ->
                Filter.SubTasksOnly
                    |> TsEncode.runExample Filter.scopeEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder Filter.scopeDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.SubTasksOnly)
        , test "can encode and decode Filter.Both" <|
            \() ->
                Filter.Both
                    |> TsEncode.runExample Filter.scopeEncoder
                    |> .output
                    |> DecodeHelpers.runDecoder Filter.scopeDecoder
                    |> .decoded
                    |> Expect.equal (Ok Filter.Both)
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


polarityFromString : Test
polarityFromString =
    describe "polarityFromString"
        [ test "converts 'Allow' into Polarity.Allow" <|
            \() ->
                "Allow"
                    |> Filter.polarityFromString
                    |> Expect.equal Filter.Allow
        , test "converts 'Deny' into Polarity.Deny" <|
            \() ->
                "Deny"
                    |> Filter.polarityFromString
                    |> Expect.equal Filter.Deny
        , test "converts 'BadInput' into Polarity.Allow" <|
            \() ->
                "BadInput"
                    |> Filter.polarityFromString
                    |> Expect.equal Filter.Allow
        ]


updatePath : Test
updatePath =
    describe "updatePath"
        [ test "updates a matching file filter" <|
            \() ->
                FilterHelpers.fileFilter "a/b/c.ext"
                    |> Filter.updatePath "a/b/c.ext" "c.ext"
                    |> Filter.value
                    |> Expect.equal "c.ext"
        , test "does not update a non-matching file filter" <|
            \() ->
                FilterHelpers.fileFilter "a/b/c.ext"
                    |> Filter.updatePath "a/b/c" "c.ext"
                    |> Filter.value
                    |> Expect.equal "a/b/c.ext"
        , test "updates a matching path filter" <|
            \() ->
                FilterHelpers.pathFilter "a/b/c"
                    |> Filter.updatePath "a/b/c" "/"
                    |> Filter.value
                    |> Expect.equal "/"
        , test "does not update a non-matching path filter" <|
            \() ->
                FilterHelpers.pathFilter "a/b/c"
                    |> Filter.updatePath "a/b/c/d" "c.ext"
                    |> Filter.value
                    |> Expect.equal "a/b/c"
        , test "does not update a tag filter" <|
            \() ->
                FilterHelpers.tagFilter "a/b/c"
                    |> Filter.updatePath "a/b/c" "newTag"
                    |> Filter.value
                    |> Expect.equal "a/b/c"
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
