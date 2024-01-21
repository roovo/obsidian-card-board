module UpdatedTaskItemTests exposing (suite)

import Expect
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import Test exposing (..)
import UpdatedTaskItem


suite : Test
suite =
    concat
        [ toString
        , toggleCompletion
        ]


toString : Test
toString =
    describe "toString"
        [ test "returns the task's original text if it has not been updated" <|
            \() ->
                "- [x] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map UpdatedTaskItem.init
                    |> Result.map UpdatedTaskItem.toString
                    |> Expect.equal (Ok "- [x] foo")
        ]


toggleCompletion : Test
toggleCompletion =
    describe "toggleCompletion"
        [ describe "NoCompletion format"
            [ test "given an INCOMPLETE item it outputs a string for a completed task with no completed tag" <|
                \() ->
                    "- [ ] foo #tag1 bar #tag2 ^12345"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map UpdatedTaskItem.init
                        |> Result.map UpdatedTaskItem.toggleCompletion
                        |> Result.map UpdatedTaskItem.toString
                        |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ^12345")
            , test "returns the original text if completion is toggled twice" <|
                \() ->
                    "- [ ] foo #tag1 bar #tag2 ^12345"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map UpdatedTaskItem.init
                        |> Result.map UpdatedTaskItem.toggleCompletion
                        |> Result.map UpdatedTaskItem.toggleCompletion
                        |> Result.map UpdatedTaskItem.toString
                        |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
            ]
        ]
