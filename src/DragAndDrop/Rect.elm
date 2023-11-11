module DragAndDrop.Rect exposing
    ( Rect
    , decoder
    )

import TsJson.Decode as TsDecode



--TYPES


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }



-- DECODE


decoder : TsDecode.Decoder Rect
decoder =
    TsDecode.succeed Rect
        |> TsDecode.andMap (TsDecode.field "x" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "y" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "width" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "height" TsDecode.float)
