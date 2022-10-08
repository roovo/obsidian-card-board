module InteropDefinitionsTests exposing (suite)

import BoardConfig
import Expect
import Filter
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import InteropDefinitions exposing (interop)
import Semver
import TagList
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ flagsTests
        , fromElmTests
        , toElmTests
        ]


flagsTests : Test
flagsTests =
    describe "interop.flags (decoding)"
        [ test "decodes valid flags for settings version 0.4.0" <|
            \() ->
                """{"now":11,"zone":22,"settings":{"version":"0.4.0","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","showFilteredTags":true,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 4 0 [] []
                                , boardConfigs =
                                    [ BoardConfig.DateBoardConfig
                                        { completedCount = 4
                                        , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                        , filterPolarity = Filter.Deny
                                        , showFilteredTags = True
                                        , includeUndated = True
                                        , title = "date board title"
                                        }
                                    , BoardConfig.TagBoardConfig
                                        { columns = [ { displayTitle = "title 1", tag = "tag 1" } ]
                                        , showColumnTags = False
                                        , completedCount = 5
                                        , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                        , filterPolarity = Filter.Allow
                                        , showFilteredTags = False
                                        , includeOthers = False
                                        , includeUntagged = True
                                        , title = "tag board title"
                                        }
                                    ]
                                }
                            , now = 11
                            , zone = 22
                            }
                        )
        , test "decodes valid flags for settings version 0.3.0" <|
            \() ->
                """{"now":11,"zone":22,"settings":{"version":"0.3.0","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 4 0 [] []
                                , boardConfigs =
                                    [ BoardConfig.DateBoardConfig
                                        { completedCount = 4
                                        , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                        , filterPolarity = Filter.Deny
                                        , showFilteredTags = True
                                        , includeUndated = True
                                        , title = "date board title"
                                        }
                                    , BoardConfig.TagBoardConfig
                                        { columns = [ { displayTitle = "title 1", tag = "tag 1" } ]
                                        , showColumnTags = True
                                        , completedCount = 5
                                        , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                        , filterPolarity = Filter.Allow
                                        , showFilteredTags = True
                                        , includeOthers = False
                                        , includeUntagged = True
                                        , title = "tag board title"
                                        }
                                    ]
                                }
                            , now = 11
                            , zone = 22
                            }
                        )
        , test "decodes valid flags for settings version 0.2.0" <|
            \() ->
                """{"now":11,"zone":22,"settings":{"version":"0.2.0","data":{"globalSettings":{"hideCompletedSubtasks":true,"ignorePaths":[{"tag":"pathFilter","data":"aPathToIgnore"}],"subTaskDisplayLimit":7},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 4 0 [] []
                                , boardConfigs =
                                    [ BoardConfig.DateBoardConfig
                                        { completedCount = 4
                                        , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                        , filterPolarity = Filter.Allow
                                        , showFilteredTags = True
                                        , includeUndated = True
                                        , title = "date board title"
                                        }
                                    , BoardConfig.TagBoardConfig
                                        { columns = [ { displayTitle = "title 1", tag = "tag 1" } ]
                                        , showColumnTags = True
                                        , completedCount = 5
                                        , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                        , filterPolarity = Filter.Allow
                                        , showFilteredTags = True
                                        , includeOthers = False
                                        , includeUntagged = True
                                        , title = "tag board title"
                                        }
                                    ]
                                }
                            , now = 11
                            , zone = 22
                            }
                        )
        , test "decodes valid flags for settings version 0.1.0" <|
            \() ->
                """{"now":11,"zone":22,"settings":{"version":"0.1.0","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 4 0 [] []
                                , boardConfigs =
                                    [ BoardConfig.DateBoardConfig
                                        { completedCount = 4
                                        , filters = []
                                        , filterPolarity = Filter.Allow
                                        , showFilteredTags = True
                                        , includeUndated = True
                                        , title = "date board title"
                                        }
                                    , BoardConfig.TagBoardConfig
                                        { columns = [ { displayTitle = "title 1", tag = "tag 1" } ]
                                        , showColumnTags = True
                                        , completedCount = 5
                                        , filters = []
                                        , filterPolarity = Filter.Allow
                                        , showFilteredTags = True
                                        , includeOthers = False
                                        , includeUntagged = True
                                        , title = "tag board title"
                                        }
                                    ]
                                }
                            , now = 11
                            , zone = 22
                            }
                        )
        , test "fails to decode flags if a field is missing" <|
            \() ->
                """{"format":"a format","now":11,"zone":22}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


fromElmTests : Test
fromElmTests =
    describe "interop.fromElm (encoding)"
        [ test "encodes AddFilePreviewHovers data" <|
            \() ->
                [ { filePath = "a path", id = "an id" } ]
                    |> InteropDefinitions.AddFilePreviewHovers
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"addFilePreviewHovers","data":[{"filePath":"a path","id":"an id"}]}"""
        , test "encodes CloseView" <|
            \() ->
                InteropDefinitions.CloseView
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"closeView"}"""
        , test "encodes DeleteTask data" <|
            \() ->
                { filePath = "a path", lineNumber = 33, originalText = "the text" }
                    |> InteropDefinitions.DeleteTask
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"deleteTask","data":{"filePath":"a path","lineNumber":33,"originalText":"the text"}}"""
        , test "encodes DisplayTaskMarkdown data" <|
            \() ->
                [ { filePath = "a path", taskMarkdown = [ { id = "an id", markdown = "some markdown" } ] } ]
                    |> InteropDefinitions.DisplayTaskMarkdown
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"displayTaskMarkdown","data":[{"filePath":"a path","taskMarkdown":[{"id":"an id","markdown":"some markdown"}]}]}"""
        , test "encodes ElmInitialized" <|
            \() ->
                InteropDefinitions.ElmInitialized
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"elmInitialized"}"""
        , test "encodes OpenTaskSourceFile data" <|
            \() ->
                { filePath = "a path", lineNumber = 33, originalText = "the text" }
                    |> InteropDefinitions.OpenTaskSourceFile
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"openTaskSourceFile","data":{"filePath":"a path","lineNumber":33,"originalText":"the text"}}"""
        , test "encodes RequestFilterCandidates" <|
            \() ->
                InteropDefinitions.RequestFilterCandidates
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"requestFilterCandidates"}"""
        , test "encodes UpdateTasks data" <|
            \() ->
                { filePath = "a path", tasks = [ { lineNumber = 12, originalText = "what was there", newText = "new text" } ] }
                    |> InteropDefinitions.UpdateTasks
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"updateTasks","data":{"filePath":"a path","tasks":[{"lineNumber":12,"originalText":"what was there","newText":"new text"}]}}"""
        ]


toElmTests : Test
toElmTests =
    describe "interop.toElm (decoding)"
        [ test "decodes activeStateUpdated data" <|
            \() ->
                """{"tag":"activeStateUpdated","data":false}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.ActiveStateUpdated False)
        , test "decodes allMarkdownLoaded" <|
            \() ->
                """{"tag":"allMarkdownLoaded","data":{}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.AllMarkdownLoaded)
        , test "decodes fileAdded data" <|
            \() ->
                """{"tag":"fileAdded","data":{"filePath":"a path","fileDate":"a date","fileContents":"---\\ntags: [ a_tag ]\\n---\\nsome contents"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal
                        (Ok <|
                            InteropDefinitions.FileAdded
                                { filePath = "a path"
                                , fileDate = Just "a date"
                                , frontMatterTags = TagList.fromList [ "a_tag" ]
                                , bodyOffset = 3
                                , body = "some contents"
                                }
                        )
        , test "decodes fileDeleted data" <|
            \() ->
                """{"tag":"fileDeleted","data":"a path"}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FileDeleted "a path")
        , test "decodes fileRenamed data" <|
            \() ->
                """{"tag":"fileRenamed","data":{"oldPath":"the old path","newPath":"the new path"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FileRenamed ( "the old path", "the new path" ))
        , test "decodes fileUpdated data" <|
            \() ->
                """{"tag":"fileUpdated","data":{"filePath":"a path","fileDate":"a date","frontMatterTags":["a_tag"],"fileContents":"---\\ntags: [ a_tag ]\\n---\\nsome contents"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal
                        (Ok <|
                            InteropDefinitions.FileUpdated
                                { filePath = "a path"
                                , fileDate = Just "a date"
                                , frontMatterTags = TagList.fromList [ "a_tag" ]
                                , bodyOffset = 3
                                , body = "some contents"
                                }
                        )
        , test "decodes filterCandidates data" <|
            \() ->
                """{"tag":"filterCandidates","data":[{"tag":"pathFilter","data":"a path"},{"tag":"pathFilter","data":"another path"}]}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FilterCandidates [ FilterHelpers.pathFilter "a path", FilterHelpers.pathFilter "another path" ])
        , test "decodes showBoard data" <|
            \() ->
                """{"tag":"showBoard","data":17}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.ShowBoard 17)
        , test "decodes version 0.4.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.4.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 4 0 [] [], boardConfigs = [] })
        , test "decodes version 0.3.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.3.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 4 0 [] [], boardConfigs = [] })
        , test "decodes version 0.2.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.2.0","data":{"boardConfigs":[],"globalSettings":{"hideCompletedSubtasks":false,"ignorePaths":[],"subTaskDisplayLimit":null}}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 4 0 [] [], boardConfigs = [] })
        , test "decodes version 0.1.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.1.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 4 0 [] [], boardConfigs = [] })
        , test "fails to decode an unsupported version of settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"99999.0.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails to decode data with an unknown tag" <|
            \() ->
                """{"tag":"xxxxx","data":{"filePath":"a path","fileDate":"a date","fileContents":"some contents"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
