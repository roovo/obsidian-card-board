module CollapseStates exposing
    ( CollapseStates
    , columnIsToggled
    , init
    , toggleColumn
    )

import List.Extra as LE



-- TYPES


type CollapseStates
    = CollapsedIndicies (List Int)


init : CollapseStates
init =
    CollapsedIndicies []



-- INFO


columnIsToggled : Int -> CollapseStates -> Bool
columnIsToggled column (CollapsedIndicies indicies) =
    List.member column indicies



-- MODIFY


toggleColumn : Int -> CollapseStates -> CollapseStates
toggleColumn column (CollapsedIndicies indicies) =
    if List.member column indicies then
        CollapsedIndicies <| LE.remove column indicies

    else
        CollapsedIndicies <| column :: indicies
