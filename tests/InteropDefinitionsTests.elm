module InteropDefinitionsTests exposing (suite)

import BoardConfig
import CardBoardSettings
import Expect
import Filter
import InteropDefinitions exposing (interop)
import Semver
import Test exposing (..)
import TsJson.Decode as TsDecode
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
        [ test "decodes valid flags for settings version 0.2.0" <|
            \() ->
                """{"now":11,"zone":22,"settings":{"version":"0.2.0","data":{"globalSettings":{"hideCompletedSubtasks":true,"ignorePaths":[{"tag":"pathFilter","data":"aPathToIgnore"}],"subTaskDisplayLimit":7},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 2 0 [] []
                                , boardConfigs =
                                    [ BoardConfig.DateBoardConfig
                                        { completedCount = 4
                                        , filters = [ Filter.PathFilter "a/path", Filter.TagFilter "tag1" ]
                                        , includeUndated = True
                                        , title = "date board title"
                                        }
                                    , BoardConfig.TagBoardConfig
                                        { columns = [ { displayTitle = "title 1", tag = "tag 1" } ]
                                        , completedCount = 5
                                        , filters = [ Filter.PathFilter "b/path", Filter.TagFilter "tag2" ]
                                        , includeOthers = False
                                        , includeUntagged = True
                                        , title = "tag board title"
                                        }
                                    ]
                                , globalSettings =
                                    { hideCompletedSubtasks = True
                                    , ignorePaths = [ Filter.PathFilter "aPathToIgnore" ]
                                    , subTaskDisplayLimit = Just 7
                                    }
                                }
                            , now = 11
                            , zone = 22
                            }
                        )
        , test "decodes valid flags for settings version 0.1.0" <|
            \() ->
                """{"now":11,"zone":22,"settings":{"version":"0.1.0","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 2 0 [] []
                                , boardConfigs =
                                    [ BoardConfig.DateBoardConfig
                                        { completedCount = 4
                                        , filters = []
                                        , includeUndated = True
                                        , title = "date board title"
                                        }
                                    , BoardConfig.TagBoardConfig
                                        { columns = [ { displayTitle = "title 1", tag = "tag 1" } ]
                                        , completedCount = 5
                                        , filters = []
                                        , includeOthers = False
                                        , includeUntagged = True
                                        , title = "tag board title"
                                        }
                                    ]
                                , globalSettings = defaultGlobalSettings
                                }
                            , now = 11
                            , zone = 22
                            }
                        )
        , test "fails to decode flags if a field is missing" <|
            \() ->
                """{"format":"a format","now":11,"zone":22}"""
                    |> runDecoder interop.flags
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
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.ActiveStateUpdated False)
        , test "decodes allMarkdownLoaded" <|
            \() ->
                """{"tag":"allMarkdownLoaded","data":{}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.AllMarkdownLoaded)
        , test "decodes fileAdded data" <|
            \() ->
                """{"tag":"fileAdded","data":{"filePath":"a path","fileDate":"a date","fileContents":"some contents"}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FileAdded { filePath = "a path", fileDate = Just "a date", fileContents = "some contents" })
        , test "decodes fileDeleted data" <|
            \() ->
                """{"tag":"fileDeleted","data":"a path"}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FileDeleted "a path")
        , test "decodes fileRenamed data" <|
            \() ->
                """{"tag":"fileRenamed","data":{"oldPath":"the old path","newPath":"the new path"}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FileRenamed ( "the old path", "the new path" ))
        , test "decodes fileUpdated data" <|
            \() ->
                """{"tag":"fileUpdated","data":{"filePath":"a path","fileDate":"a date","fileContents":"some contents"}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FileUpdated { filePath = "a path", fileDate = Just "a date", fileContents = "some contents" })
        , test "decodes showBoard data" <|
            \() ->
                """{"tag":"showBoard","data":17}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.ShowBoard 17)
        , test "decodes version 0.2.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.2.0","data":{"boardConfigs":[],"globalSettings":{"hideCompletedSubtasks":false,"ignorePaths":[],"subTaskDisplayLimit":null}}}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 2 0 [] [], boardConfigs = [], globalSettings = defaultGlobalSettings })
        , test "decodes version 0.1.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.1.0","data":{"boardConfigs":[]}}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 2 0 [] [], boardConfigs = [], globalSettings = defaultGlobalSettings })
        , test "fails to decode an unsupported version of settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"99999.0.0","data":{"boardConfigs":[]}}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails to decode data with an unknown tag" <|
            \() ->
                """{"tag":"xxxxx","data":{"filePath":"a path","fileDate":"a date","fileContents":"some contents"}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]



-- HELPERS


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


defaultGlobalSettings : CardBoardSettings.GlobalSettings
defaultGlobalSettings =
    { hideCompletedSubtasks = False
    , ignorePaths = []
    , subTaskDisplayLimit = Nothing
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
