module Helpers.BoardHelpers exposing
    ( cardsInColumn
    , tasksInColumn
    )

import Card exposing (Card)
import TaskItem exposing (TaskItem)


cardsInColumn : String -> List ( String, List Card ) -> List Card
cardsInColumn columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\( c, _ ) -> c == columnName)
        |> List.concatMap Tuple.second


tasksInColumn : String -> List ( String, List TaskItem ) -> List TaskItem
tasksInColumn columnName tasksInColumns =
    tasksInColumns
        |> List.filter (\( c, _ ) -> c == columnName)
        |> List.concatMap Tuple.second
