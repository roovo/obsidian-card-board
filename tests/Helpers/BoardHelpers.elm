module Helpers.BoardHelpers exposing
    ( columnTitled
    , thingsInColumn
    , thingsInColumns
    )

import Card exposing (Card)
import Column exposing (Column)


columnTitled : String -> List Column -> Maybe Column
columnTitled columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\c -> Column.name c == columnName)
        |> List.head


thingsInColumns : List String -> List Column -> List Card
thingsInColumns columnNames cardsInColumns =
    List.concatMap (\columnName -> thingsInColumn columnName cardsInColumns) columnNames


thingsInColumn : String -> List Column -> List Card
thingsInColumn columnName cardsInColumns =
    cardsInColumns
        |> List.filter (\c -> Column.name c == columnName)
        |> List.concatMap (Column.cards "")
