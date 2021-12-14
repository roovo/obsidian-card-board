module Helpers.TaskItemHelpers exposing (exampleTaskItem)

import Parser
import TaskItem exposing (TaskItem)


exampleTaskItem : String -> String -> TaskItem
exampleTaskItem markdown path =
    Parser.run (TaskItem.parser path Nothing) markdown
        |> Result.withDefault TaskItem.dummy
