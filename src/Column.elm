module Column exposing
    ( Column
    , PlacementResult(..)
    , hasName
    , init
    , isEmpty
    , items
    , name
    )

-- TYPES


type Column a
    = Column String (List a)


type PlacementResult
    = CompletedInThisColumn
    | DoesNotBelong
    | Placed



-- CONSTRUCTION


init : String -> List a -> Column a
init name_ items_ =
    Column name_ items_



-- INFO


hasName : String -> Column a -> Bool
hasName n (Column name_ _) =
    n == name_


isEmpty : Column a -> Bool
isEmpty =
    List.isEmpty << items


items : Column a -> List a
items (Column _ items_) =
    items_


name : Column a -> String
name (Column name_ _) =
    name_
