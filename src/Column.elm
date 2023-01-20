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
    = Column (Config a)


type alias Config a =
    { enabled : Bool
    , name : String
    , items : List a
    }


type PlacementResult
    = CompletedInThisColumn
    | DoesNotBelong
    | Placed



-- CONSTRUCTION


init : Bool -> String -> List a -> Column a
init enabled_ name_ items_ =
    Column
        { enabled = enabled_
        , name = name_
        , items = items_
        }



-- INFO


hasName : String -> Column a -> Bool
hasName n =
    (==) n << name


isEmpty : Column a -> Bool
isEmpty =
    List.isEmpty << items


isEnabled : Column a -> Bool
isEnabled =
    .enabled << config


items : Column a -> List a
items =
    .items << config


name : Column a -> String
name =
    .name << config



-- PRIVATE


config : Column a -> Config a
config (Column c) =
    c
