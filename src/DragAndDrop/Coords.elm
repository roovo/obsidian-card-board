module DragAndDrop.Coords exposing
    ( Coords
    , Plane(..)
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


type Plane
    = Both
    | Horizontal
    | Vertical



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


distance : Plane -> Coords -> Coords -> Float
distance plane coords1 coords2 =
    let
        dx : Float
        dx =
            coords1.x - coords2.x

        dy : Float
        dy =
            coords1.y - coords2.y
    in
    case plane of
        Both ->
            sqrt ((dx ^ 2) + (dy ^ 2))

        Horizontal ->
            abs dx

        Vertical ->
            abs dy


subtract : Coords -> Coords -> Coords
subtract coords1 coords2 =
    { x = coords1.x - coords2.x
    , y = coords1.y - coords2.y
    }
