module Worker.InteropDefinitionsTests exposing (suite)

import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.TaskItemHelpers exposing (safeTaskItem)
import TagList
import TaskList
import Test exposing (..)
import TsJson.Encode as TsEncode
import Worker.InteropDefinitions as InteropDefinitions exposing (interop)


suite : Test
suite =
    concat
        [ fromElmTests
        , toElmTests
        ]


fromElmTests : Test
fromElmTests =
    describe "interop.fromElm (encoding)"
        [ test "encodes AllTasksLoaded" <|
            \() ->
                InteropDefinitions.AllTasksLoaded
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"allTasksLoaded"}"""
        , test "encodes AllTaskItems with an empty TaskList" <|
            \() ->
                TaskList.empty
                    |> InteropDefinitions.AllTaskItems
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"allTaskItems","data":[]}"""
        , test "encodes AllTaskItems with an non-empty TaskList" <|
            \() ->
                [ safeTaskItem "- [ ] foo", safeTaskItem "- [ ] bar" ]
                    |> TaskList.fromList
                    |> InteropDefinitions.AllTaskItems
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"allTaskItems","data":[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]},{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]}]}"""
        , test "encodes TasksAdded with an empty TaskList" <|
            \() ->
                TaskList.empty
                    |> InteropDefinitions.TasksAdded
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksAdded","data":[]}"""
        , test "encodes TasksAdded with an non-empty TaskList" <|
            \() ->
                [ safeTaskItem "- [ ] foo", safeTaskItem "- [ ] bar" ]
                    |> TaskList.fromList
                    |> InteropDefinitions.TasksAdded
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksAdded","data":[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]},{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]}]}"""
        , test "encodes TasksDeleted with no TaskItems" <|
            \() ->
                []
                    |> InteropDefinitions.TasksDeleted
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksDeleted","data":[]}"""
        , test "encodes TasksDeleted with some TaskItems" <|
            \() ->
                [ safeTaskItem "- [ ] foo", safeTaskItem "- [ ] bar" ]
                    |> InteropDefinitions.TasksDeleted
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksDeleted","data":["2166136261:1","2166136261:1"]}"""
        , test "encodes TasksDeletedAndAdded with no TaskItems" <|
            \() ->
                ( [], [] )
                    |> InteropDefinitions.TasksDeletedAndAdded
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksDeletedAndAdded","data":[[],[]]}"""
        , test "encodes TasksDeletedAndAdded with some TaskItems" <|
            \() ->
                ( [ safeTaskItem "- [ ] foo", safeTaskItem "- [ ] bar" ], [ safeTaskItem "- [ ] baz" ] )
                    |> InteropDefinitions.TasksDeletedAndAdded
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksDeletedAndAdded","data":[[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]},{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]}],[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"baz"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] baz","tags":[],"title":["baz"]},"subFields":[]}]]}"""
        , test "encodes TasksUpdated with no TaskItems" <|
            \() ->
                []
                    |> InteropDefinitions.TasksUpdated
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksUpdated","data":[]}"""
        , test "encodes TasksUpdated with some TaskItems" <|
            \() ->
                [ ( "foo", safeTaskItem "- [ ] foo" ), ( "bar", safeTaskItem "- [ ] bar" ) ]
                    |> InteropDefinitions.TasksUpdated
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksUpdated","data":[["foo",{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]}],["bar",{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalLine":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]}]]}"""
        ]


toElmTests : Test
toElmTests =
    describe "interop.toElm (decoding)"
        [ test "decodes allMarkdownLoaded" <|
            \() ->
                """{"tag":"allMarkdownLoaded","data":{}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.AllMarkdownLoaded)
        , test "decodes viewInitialized" <|
            \() ->
                """{"tag":"viewInitialized"}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok InteropDefinitions.ViewInitialized)
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
        , test "decodes fileModified data" <|
            \() ->
                """{"tag":"fileModified","data":{"filePath":"a path","fileDate":"a date","frontMatterTags":["a_tag"],"fileContents":"---\\ntags: [ a_tag ]\\n---\\nsome contents"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal
                        (Ok <|
                            InteropDefinitions.FileModified
                                { filePath = "a path"
                                , fileDate = Just "a date"
                                , frontMatterTags = TagList.fromList [ "a_tag" ]
                                , bodyOffset = 3
                                , body = "some contents"
                                }
                        )
        , test "decodes fileRenamed data" <|
            \() ->
                """{"tag":"fileRenamed","data":{"oldPath":"the old path","newPath":"the new path"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FileRenamed ( "the old path", "the new path" ))
        , test "fails to decode data with an unknown tag" <|
            \() ->
                """{"tag":"xxxxx","data":{"filePath":"a path","fileDate":"a date","fileContents":"some contents"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]
