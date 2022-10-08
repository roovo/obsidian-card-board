module Helpers.BoardHelpers exposing
    ( thingsInColumn
    , thingsInColumns
    )

import Column exposing (Column)


thingsInColumns : List String -> List (Column a) -> List a
thingsInColumns columnNames cardsInColumns =
    List.concatMap (\columnName -> thingsInColumn columnName cardsInColumns) columnNames


thingsInColumn : String -> List (Column a) -> List a
thingsInColumn columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\c -> Column.hasName columnName c)
        |> List.concatMap Column.items
