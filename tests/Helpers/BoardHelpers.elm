module Helpers.BoardHelpers exposing (thingsInColumn)

import Card exposing (Card)
import Column exposing (Column)
import TaskItem exposing (TaskItem)


thingsInColumn : String -> List (Column a) -> List a
thingsInColumn columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\c -> Column.hasName columnName c)
        |> List.concatMap Column.items
