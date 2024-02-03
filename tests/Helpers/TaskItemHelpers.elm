module Helpers.TaskItemHelpers exposing
    ( basicParser
    , exampleTaskItem
    , safeTaskItem
    )

import DataviewTaskCompletion
import Parser exposing (Parser)
import TagList
import TaskItem exposing (TaskItem)


basicParser : Parser TaskItem
basicParser =
    TaskItem.parser (DataviewTaskCompletion.Text "completion") "" Nothing TagList.empty 0


exampleTaskItem : String -> String -> TaskItem
exampleTaskItem markdown path =
    Parser.run (TaskItem.parser DataviewTaskCompletion.NoCompletion path Nothing TagList.empty 0) markdown
        |> Result.withDefault TaskItem.dummy


safeTaskItem : String -> TaskItem
safeTaskItem markdown =
    Parser.run basicParser markdown
        |> Result.withDefault TaskItem.dummy
