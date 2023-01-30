module CollapseStates exposing
    ( CollapseStates
    , collapseColumn
    , columnIsCollapsed
    , decoder
    , encoder
    , init
    )

import List.Extra as LE
import Set exposing (Set)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type CollapseStates
    = CollapseStates (Set Int)


init : CollapseStates
init =
    CollapseStates Set.empty



-- SERIALIZATION


decoder : TsDecode.Decoder CollapseStates
decoder =
    TsDecode.list TsDecode.int
        |> TsDecode.map Set.fromList
        |> TsDecode.map CollapseStates


encoder : TsEncode.Encoder CollapseStates
encoder =
    TsEncode.list TsEncode.int
        |> TsEncode.map Set.toList
        |> TsEncode.map states



-- INFO


columnIsCollapsed : Int -> CollapseStates -> Bool
columnIsCollapsed column collapseStates =
    Set.member column (states collapseStates)



-- MODIFY


collapseColumn : Int -> Bool -> CollapseStates -> CollapseStates
collapseColumn column state collapseStates =
    if state then
        CollapseStates <| Set.insert column (states collapseStates)

    else
        CollapseStates <| Set.remove column (states collapseStates)



-- PRIVATE


states : CollapseStates -> Set Int
states (CollapseStates s) =
    s
