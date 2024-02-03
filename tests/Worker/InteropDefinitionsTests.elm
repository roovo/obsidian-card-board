module Worker.InteropDefinitionsTests exposing (suite)

import Expect
import Helpers.TaskItemHelpers exposing (safeTaskItem)
import TaskList
import Test exposing (..)
import TsJson.Encode as TsEncode
import Worker.InteropDefinitions as InteropDefinitions exposing (interop)


suite : Test
suite =
    concat
        [ fromElmTests
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
                    |> Expect.equal """{"tag":"tasksAdded","data":[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalText":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]},{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalText":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]}]}"""
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
                    |> TaskList.fromList
                    |> TaskList.toList
                    |> InteropDefinitions.TasksDeleted
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"tasksDeleted","data":["2166136261:1","2166136261:1"]}"""
        ]
