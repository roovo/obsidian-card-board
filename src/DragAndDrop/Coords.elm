module DragAndDrop.Coords exposing
    ( Coords
    , decoder
    )

import TsJson.Decode as TsDecode



-- TYPES


type alias Coords =
    { x : Float
    , y : Float
    }



-- DECODE


decoder : TsDecode.Decoder Coords
decoder =
    TsDecode.succeed Coords
        |> TsDecode.andMap (TsDecode.field "x" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "y" TsDecode.float)
