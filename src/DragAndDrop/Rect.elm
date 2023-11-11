module DragAndDrop.Rect exposing
    ( Rect
    , centre
    , closestTo
    , decoder
    )

import DragAndDrop.Coords as Coords exposing (Coords)
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



-- UTILS


centre : Rect -> Coords
centre { x, y, width, height } =
    { x = x + (width / 2)
    , y = y + (height / 2)
    }


closestTo : Coords -> List { id : a, rect : Rect } -> Maybe a
closestTo target rects =
    rects
        |> List.map (\r -> ( r.id, r.rect ))
        |> List.map (Tuple.mapSecond (Coords.distance target << centre))
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first
