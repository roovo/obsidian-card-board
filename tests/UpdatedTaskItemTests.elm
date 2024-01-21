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
