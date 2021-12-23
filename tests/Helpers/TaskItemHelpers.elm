module Helpers.TaskItemHelpers exposing
    ( basicParser
    , exampleTaskItem
    )

import Parser exposing (Parser)
import Set
import TaskItem exposing (TaskItem)


basicParser : Parser TaskItem
basicParser =
    TaskItem.parser "" Nothing Set.empty 0


exampleTaskItem : String -> String -> TaskItem
exampleTaskItem markdown path =
    Parser.run (TaskItem.parser path Nothing Set.empty 0) markdown
        |> Result.withDefault TaskItem.dummy
