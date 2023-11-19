module Column exposing
    ( Column
    , PlacementResult(..)
    , collapseState
    , hasName
    , init
    , isCollapsed
    , isEmpty
    , isEnabled
    , items
    , name
    , tagsToHide
    )

-- TYPES


type Column a
    = Column (Config a)


type alias Config a =
    { enabled : Bool
    , name : String
    , items : List a
    , collapsed : Bool
    , tagsToHide : List String
    }


type PlacementResult
    = CompletedInThisColumn
    | DoesNotBelong
    | Placed



-- CONSTRUCTION


init : Bool -> String -> List String -> List a -> Column a
init enabled_ name_ tagsToHide_ items_ =
    Column
        { enabled = enabled_
        , name = name_
        , items = items_
        , collapsed = False
        , tagsToHide = tagsToHide_
        }



-- INFO


hasName : String -> Column a -> Bool
hasName n =
    (==) n << name


isCollapsed : Column a -> Bool
isCollapsed =
    .collapsed << config


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


tagsToHide : Column a -> List String
tagsToHide =
    .tagsToHide << config



-- MODIFICATION


collapseState : Bool -> Column a -> Column a
collapseState collapsed (Column c) =
    Column { c | collapsed = collapsed }



-- PRIVATE


config : Column a -> Config a
config (Column c) =
    c
