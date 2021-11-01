module InteropDefinitionsTests exposing (suite)

import CardBoardConfig
import Expect
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
        [ test "decodes valid flags" <|
            \() ->
                """{"now":11,"zone":22,"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}"""
                    |> runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { boardConfigs =
                                [ CardBoardConfig.DateBoardConfig
                                    { completedCount = 4
                                    , includeUndated = True
                                    , title = "date board title"
                                    }
                                , CardBoardConfig.TagBoardConfig
                                    { columns = [ { displayTitle = "title 1", tag = "tag 1" } ]
                                    , completedCount = 5
                                    , includeOthers = False
                                    , includeUntagged = True
                                    , title = "tag board title"
                                    }
                                ]
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
        , test "builds the correct tsType" <|
            \() ->
                ""
                    |> runDecoder interop.flags
                    |> .tsType
                    |> Expect.equal
                        ("{ boardConfigs : ({ data : { completedCount : number; includeUndated : boolean; title : string }; tag : \"dateBoardConfig\" } | { data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : \"tagBoardConfig\" })[]; "
                            ++ "now : number; "
                            ++ "zone : number }"
                        )
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
        , test "encodes DeleteTodo data" <|
            \() ->
                { filePath = "a path", lineNumber = 33, originalText = "the text" }
                    |> InteropDefinitions.DeleteTodo
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"deleteTodo","data":{"filePath":"a path","lineNumber":33,"originalText":"the text"}}"""
        , test "encodes DisplayTodoMarkdown data" <|
            \() ->
                [ { filePath = "a path", todoMarkdown = [ { id = "an id", markdown = "some markdown" } ] } ]
                    |> InteropDefinitions.DisplayTodoMarkdown
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"displayTodoMarkdown","data":[{"filePath":"a path","todoMarkdown":[{"id":"an id","markdown":"some markdown"}]}]}"""
        , test "encodes OpenTodoSourceFile data" <|
            \() ->
                { filePath = "a path", lineNumber = 33, originalText = "the text" }
                    |> InteropDefinitions.OpenTodoSourceFile
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"openTodoSourceFile","data":{"filePath":"a path","lineNumber":33,"originalText":"the text"}}"""
        , test "encodes UpdateSettings with DateBoardConfig data" <|
            \() ->
                { version = Semver.version 1 2 3 [] []
                , boardConfigs =
                    [ CardBoardConfig.DateBoardConfig
                        { completedCount = 3
                        , includeUndated = True
                        , title = "A Date Board"
                        }
                    ]
                }
                    |> InteropDefinitions.UpdateSettings
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"updateSettings","data":{"version":"1.2.3","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":3,"includeUndated":true,"title":"A Date Board"}}]}}}"""
        , test "encodes UpdateTodos data" <|
            \() ->
                { filePath = "a path", todos = [ { lineNumber = 12, originalText = "what was there", newText = "new text" } ] }
                    |> InteropDefinitions.UpdateTodos
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"updateTodos","data":{"filePath":"a path","todos":[{"lineNumber":12,"originalText":"what was there","newText":"new text"}]}}"""
        , test "encodes the correct tsType" <|
            \() ->
                [ { filePath = "a path", todoMarkdown = [] } ]
                    |> InteropDefinitions.DisplayTodoMarkdown
                    |> TsEncode.runExample interop.fromElm
                    |> .tsType
                    |> Expect.equal
                        ("""{ data : { filePath : string; todos : { lineNumber : number; newText : string; originalText : string }[] }; tag : "updateTodos" }"""
                            ++ """ | { data : { data : { boardConfigs : ({ data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" } | { data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" })[] }; version : string }; tag : "updateSettings" }"""
                            ++ """ | { data : { filePath : string; lineNumber : number; originalText : string }; tag : "openTodoSourceFile" }"""
                            ++ """ | { data : { filePath : string; todoMarkdown : { id : string; markdown : string }[] }[]; tag : "displayTodoMarkdown" }"""
                            ++ """ | { data : { filePath : string; lineNumber : number; originalText : string }; tag : "deleteTodo" }"""
                            ++ """ | { data : { filePath : string; id : string }[]; tag : "addFilePreviewHovers" }"""
                        )
        ]


toElmTests : Test
toElmTests =
    describe "interop.toElm (decoding)"
        [ test "decodes fileAdded data" <|
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
        , test "decodes version 0.1.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.1.0","data":{"boardConfigs":[]}}}"""
                    |> runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 1 0 [] [], boardConfigs = [] })
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
        , test "builds the correct tsType" <|
            \() ->
                ""
                    |> runDecoder interop.toElm
                    |> .tsType
                    |> Expect.equal
                        ("{ data : { fileContents : string; fileDate : string | null; filePath : string }; tag : \"fileAdded\" }"
                            ++ " | { data : string; tag : \"fileDeleted\" }"
                            ++ " | { data : { newPath : string; oldPath : string }; tag : \"fileRenamed\" }"
                            ++ " | { data : { fileContents : string; fileDate : string | null; filePath : string }; tag : \"fileUpdated\" }"
                            ++ " | { data : ({ version : string } & ({ data : JsonValue } | { data : { boardConfigs : ({ data : { completedCount : number; includeUndated : boolean; title : string }; tag : \"dateBoardConfig\" } | { data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : \"tagBoardConfig\" })[] } })); tag : \"settingsUpdated\" }"
                            ++ " | { data : JsonValue; tag : \"initCompleted\" }"
                        )
        ]



-- HELPERS


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
