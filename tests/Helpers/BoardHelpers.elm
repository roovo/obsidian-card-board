module Helpers.BoardHelpers exposing (thingsInColumn)

import Column exposing (Column)


thingsInColumn : String -> List (Column a) -> List a
thingsInColumn columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\c -> Column.hasName columnName c)
        |> List.concatMap Column.items
