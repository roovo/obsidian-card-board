module DragAndDrop.Coords exposing
    ( Coords
    , decoder
    , encoder
    , fromFloatTuple
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
