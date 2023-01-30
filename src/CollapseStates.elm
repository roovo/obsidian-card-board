module CollapseStates exposing
    ( CollapseStates
    , collapseColumn
    , columnIsCollapsed
    , init
    )

import Dict exposing (Dict)
import List.Extra as LE



-- TYPES


type CollapseStates
    = CollapseStates (Dict Int Bool)


init : CollapseStates
init =
    CollapseStates Dict.empty



-- INFO


columnIsCollapsed : Int -> CollapseStates -> Bool
columnIsCollapsed column (CollapseStates states) =
    Dict.get column states
        |> Maybe.withDefault False



-- MODIFY


collapseColumn : Int -> Bool -> CollapseStates -> CollapseStates
collapseColumn column state (CollapseStates states) =
    CollapseStates <| Dict.insert column state states
