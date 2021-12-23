module Helpers.TaskItemHelpers exposing
    ( basicParser
    , exampleTaskItem
    )

import Parser exposing (Parser)
import TaskItem exposing (TaskItem)


basicParser : Parser TaskItem
basicParser =
    TaskItem.parser "" Nothing [] 0


exampleTaskItem : String -> String -> TaskItem
exampleTaskItem markdown path =
    Parser.run (TaskItem.parser path Nothing [] 0) markdown
        |> Result.withDefault TaskItem.dummy
