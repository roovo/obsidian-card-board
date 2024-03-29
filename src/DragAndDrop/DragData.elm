module DragAndDrop.DragData exposing
    ( BeaconData
    , DragAction(..)
    , DragData
    , decoder
    )

import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords exposing (Coords)
import DragAndDrop.Rect as Rect exposing (Rect)
import TsJson.Decode as TsDecode



-- TYPES


type alias DragData =
    { dragType : String
    , dragAction : DragAction
    , cursor : Coords
    , offset : Coords
    , draggedNodeRect : Rect
    , beacons : List BeaconData
    }


type DragAction
    = Move
    | Stop


type alias BeaconData =
    { beaconPosition : BeaconPosition
    , rect : Rect
    }



-- DECODERS


decoder : TsDecode.Decoder DragData
decoder =
    TsDecode.succeed DragData
        |> TsDecode.andMap (TsDecode.field "dragType" TsDecode.string)
        |> TsDecode.andMap dragActionDecoder
        |> TsDecode.andMap (TsDecode.field "cursor" Coords.decoder)
        |> TsDecode.andMap (TsDecode.field "offset" Coords.decoder)
        |> TsDecode.andMap (TsDecode.field "draggedNodeRect" Rect.decoder)
        |> TsDecode.andMap (TsDecode.field "beacons" (TsDecode.list beaconDataDecoder))



-- HELPERS


beaconDataDecoder : TsDecode.Decoder BeaconData
beaconDataDecoder =
    TsDecode.succeed BeaconData
        |> TsDecode.andMap (TsDecode.field "beaconPosition" BeaconPosition.decoder)
        |> TsDecode.andMap (TsDecode.field "rect" Rect.decoder)


dragActionDecoder : TsDecode.Decoder DragAction
dragActionDecoder =
    TsDecode.discriminatedUnion "dragAction"
        [ ( "move", TsDecode.succeed Move )
        , ( "stop", TsDecode.succeed Stop )
        ]
