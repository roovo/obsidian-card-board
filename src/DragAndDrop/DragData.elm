module DragAndDrop.DragData exposing
    ( BeaconData
    , DragData
    , DragItem(..)
    , decoder
    )

import DragAndDrop.BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords exposing (Coords)
import TsJson.Decode as TsDecode



-- TYPES


type alias DragData =
    { beaconIdentifier : String
    , dragType : String
    , cursor : Coords
    , beacons : List BeaconData
    }


type alias BeaconData =
    { id : { identifier : String, position : String }
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type DragItem
    = TabHeader String


type alias BeaconId =
    { identifier : String
    , position : String
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
        |> TsDecode.andMap (TsDecode.field "id" beaconIdDecoder)
        |> TsDecode.andMap (TsDecode.field "x" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "y" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "width" TsDecode.float)
        |> TsDecode.andMap (TsDecode.field "height" TsDecode.float)


beaconIdDecoder : TsDecode.Decoder BeaconId
beaconIdDecoder =
    TsDecode.succeed BeaconId
        |> TsDecode.andMap (TsDecode.field "identifier" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "position" TsDecode.string)
