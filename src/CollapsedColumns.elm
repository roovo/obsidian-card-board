module CollapsedColumns exposing
    ( CollapsedColumns
    , collapseColumn
    , columnIsCollapsed
    , decoder
    , encoder
    , init
    )

import Set exposing (Set)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type CollapsedColumns
    = CollapsedColumns (Set Int)


init : CollapsedColumns
init =
    CollapsedColumns Set.empty



-- SERIALIZATION


decoder : TsDecode.Decoder CollapsedColumns
decoder =
    TsDecode.list TsDecode.int
        |> TsDecode.map Set.fromList
        |> TsDecode.map CollapsedColumns


encoder : TsEncode.Encoder CollapsedColumns
encoder =
    TsEncode.list TsEncode.int
        |> TsEncode.map Set.toList
        |> TsEncode.map indicies



-- INFO


columnIsCollapsed : Int -> CollapsedColumns -> Bool
columnIsCollapsed column collapsedColumns =
    Set.member column (indicies collapsedColumns)



-- MODIFY


collapseColumn : Int -> Bool -> CollapsedColumns -> CollapsedColumns
collapseColumn column state collapsedColumns =
    if state then
        CollapsedColumns <| Set.insert column (indicies collapsedColumns)

    else
        CollapsedColumns <| Set.remove column (indicies collapsedColumns)



-- PRIVATE


indicies : CollapsedColumns -> Set Int
indicies (CollapsedColumns s) =
    s
