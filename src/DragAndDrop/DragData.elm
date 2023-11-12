module DragAndDrop.DragData exposing
    ( BeaconData
    , DragAction(..)
    , DragData
    , DragTracker
    , Size
    , decoder
    )

import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords exposing (Coords)
import DragAndDrop.Rect as Rect exposing (Rect)
import Json.Encode as JE
import TsJson.Decode as TsDecode



-- TYPES


type alias DragData =
    { beaconIdentifier : String
    , dragAction : DragAction
    , cursor : Coords
    , offset : Coords
    , draggedNodeSize : Size
    , beacons : List BeaconData
    }


type alias Size =
    { width : Float
    , height : Float
    }


type DragAction
    = Move
    | Stop


type alias BeaconData =
    { beaconPosition : BeaconPosition
    , rect : Rect
    }


type alias DragTracker =
    { nodeId : String
    , clientPos : Coords
    , offsetPos : Coords
    , offset : Coords
    , draggedNodeSize : Size
    }



-- DECODERS


decoder : TsDecode.Decoder DragData
decoder =
    TsDecode.succeed DragData
        |> TsDecode.andMap (TsDecode.field "beaconIdentifier" TsDecode.string)
        |> TsDecode.andMap dragActionDecoder
        |> TsDecode.andMap (TsDecode.field "cursor" Coords.decoder)
        |> TsDecode.andMap (TsDecode.field "offset" Coords.decoder)
        |> TsDecode.andMap (TsDecode.field "draggedNodeSize" sizeDecoder)
        |> TsDecode.andMap (TsDecode.field "beacons" (TsDecode.list beaconDataDecoder))



-- HELPERS


beaconDataDecoder : TsDecode.Decoder BeaconData
beaconDataDecoder =
    TsDecode.succeed BeaconData
        |> TsDecode.andMap (TsDecode.field "beaconPosition" beaconPositionDecoder)
        |> TsDecode.andMap (TsDecode.field "rect" Rect.decoder)


beaconPositionDecoder : TsDecode.Decoder BeaconPosition
beaconPositionDecoder =
    TsDecode.oneOf
        [ toElmBeacon "after" BeaconPosition.After TsDecode.string
        , toElmBeacon "before" BeaconPosition.Before TsDecode.string
        ]


dragActionDecoder : TsDecode.Decoder DragAction
dragActionDecoder =
    TsDecode.discriminatedUnion "dragAction"
        [ ( "move", TsDecode.succeed Move )
        , ( "stop", TsDecode.succeed Stop )
        ]


sizeDecoder : TsDecode.Decoder Size
sizeDecoder =
    TsDecode.succeed Size
        |> TsDecode.andMap (TsDecode.field "width" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "height" TsDecode.float)


toElmBeacon : String -> (value -> a) -> TsDecode.Decoder value -> TsDecode.Decoder a
toElmBeacon tagName constructor decoder_ =
    TsDecode.field "position" (TsDecode.literal constructor (JE.string tagName))
        |> TsDecode.andMap (TsDecode.field "identifier" decoder_)
