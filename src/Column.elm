module Column exposing
    ( Column
    , PlacementResult(..)
    , hasName
    , init
    , isEmpty
    , isEnabled
    , items
    , name
    )

-- TYPES


type Column a
    = Column Bool String (List a)


type PlacementResult
    = CompletedInThisColumn
    | DoesNotBelong
    | Placed



-- CONSTRUCTION


init : Bool -> String -> List a -> Column a
init enabled_ name_ items_ =
    Column enabled_ name_ items_



-- INFO


hasName : String -> Column a -> Bool
hasName n (Column _ name_ _) =
    n == name_


isEmpty : Column a -> Bool
isEmpty =
    List.isEmpty << items


isEnabled : Column a -> Bool
isEnabled (Column e _ _) =
    e


items : Column a -> List a
items (Column _ _ items_) =
    items_


name : Column a -> String
name (Column _ name_ _) =
    name_
