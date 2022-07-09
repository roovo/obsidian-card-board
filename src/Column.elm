module Column exposing
    ( Column
    , hasName
    , init
    , items
    , name
    )

-- TYPES


type Column a
    = Column String (List a)



-- CONSTRUCTION


init : String -> List a -> Column a
init name_ items_ =
    Column name_ items_



-- INFO


items : Column a -> List a
items (Column _ items_) =
    items_


name : Column a -> String
name (Column name_ _) =
    name_


hasName : String -> Column a -> Bool
hasName n (Column name_ _) =
    n == name_
