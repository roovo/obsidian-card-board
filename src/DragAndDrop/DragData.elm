module DragAndDrop.DragData exposing
    ( BeaconData
    , DragData
    , DragTracker
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
    , dragType : String
    , cursor : Coords
    , beacons : List BeaconData
    }


type alias BeaconData =
    { beaconPosition : BeaconPosition
    , rect : Rect
    }


type alias DragTracker =
    { nodeId : String
    , clientPos : Coords
    , offsetPos : Coords
    }



-- DECODERS


decoder : TsDecode.Decoder DragData
decoder =
    TsDecode.succeed DragData
        |> TsDecode.andMap (TsDecode.field "beaconIdentifier" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "dragType" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "cursor" Coords.decoder)
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


toElmBeacon : String -> (value -> a) -> TsDecode.Decoder value -> TsDecode.Decoder a
toElmBeacon tagName constructor decoder_ =
    TsDecode.field "position" (TsDecode.literal constructor (JE.string tagName))
        |> TsDecode.andMap (TsDecode.field "identifier" decoder_)
