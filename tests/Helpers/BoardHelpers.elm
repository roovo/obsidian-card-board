module Helpers.BoardHelpers exposing
    ( columnTitled
    , thingsInColumn
    , thingsInColumns
    )

import Column exposing (Column)


columnTitled : String -> List (Column a) -> Maybe (Column a)
columnTitled columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\c -> Column.hasName columnName c)
        |> List.head


thingsInColumns : List String -> List (Column a) -> List a
thingsInColumns columnNames cardsInColumns =
    List.concatMap (\columnName -> thingsInColumn columnName cardsInColumns) columnNames


thingsInColumn : String -> List (Column a) -> List a
thingsInColumn columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\c -> Column.hasName columnName c)
        |> List.concatMap Column.items
