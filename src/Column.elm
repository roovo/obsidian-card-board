module Column exposing
    ( Column
    , PlacementResult(..)
    , hasName
    , init
    , invertCollapseState
    , isCollapsed
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
    , invertCollapsed : Bool
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
        , invertCollapsed = False
        }



-- INFO


hasName : String -> Column a -> Bool
hasName n =
    (==) n << name


isCollapsed : Column a -> Bool
isCollapsed ((Column c) as col) =
    xor c.invertCollapsed (isEmpty col)


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



-- MODIFICATION


invertCollapseState : Column a -> Column a
invertCollapseState (Column c) =
    Column { c | invertCollapsed = True }



-- PRIVATE


config : Column a -> Config a
config (Column c) =
    c
