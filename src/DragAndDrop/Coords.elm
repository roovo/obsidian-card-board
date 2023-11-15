module DragAndDrop.Coords exposing
    ( Coords
    , decoder
    , distance
    , encoder
    , fromFloatTuple
    , subtract
    )

import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode exposing (required)



-- TYPES


type alias Coords =
    { x : Float
    , y : Float
    }



-- DECODE / ENCODE


decoder : TsDecode.Decoder Coords
decoder =
    TsDecode.succeed Coords
        |> TsDecode.andMap (TsDecode.field "x" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "y" TsDecode.float)


encoder : TsEncode.Encoder Coords
encoder =
    TsEncode.object
        [ required "x" .x TsEncode.float
        , required "y" .y TsEncode.float
        ]



-- CONVERSION


fromFloatTuple : ( Float, Float ) -> Coords
fromFloatTuple ( x, y ) =
    { x = x, y = y }



-- UTILS


distance : Coords -> Coords -> Float
distance coords1 coords2 =
    let
        dx : Float
        dx =
            coords1.x - coords2.x

        dy : Float
        dy =
            coords1.y - coords2.y
    in
    sqrt ((dx ^ 2) + (dy ^ 2))


subtract : Coords -> Coords -> Coords
subtract coords1 coords2 =
    { x = coords1.x - coords2.x
    , y = coords1.y - coords2.y
    }
