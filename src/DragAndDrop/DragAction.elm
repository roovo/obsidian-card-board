module DragAndDrop.DragAction exposing
    ( DragAction(..)
    , fromDragData
    )

import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords exposing (Coords)
import DragAndDrop.DragData exposing (BeaconData, DragData, DragItem)
import Maybe.Extra as ME
import TsJson.Decode as TsDecode



-- TYPES


type DragAction
    = Move Positions
    | Stop
    | NoOp


type alias Positions =
    { cursor : Coords
    , beacons : List Beacon
    }


type alias Beacon =
    { beaconPosition : BeaconPosition
    , rect : Rect
    }


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }



-- CONVERTERS


fromDragData : DragItem -> DragData -> DragAction
fromDragData draggedItem dragData =
    case dragData.dragType of
        "stop" ->
            Stop

        "move" ->
            Move
                { cursor = dragData.cursor
                , beacons =
                    dragData.beacons
                        |> List.map beaconConverter
                        |> ME.values
                }

        _ ->
            NoOp



-- HELPERS


beaconConverter : BeaconData -> Maybe Beacon
beaconConverter beaconData =
    let
        buildBeacon : BeaconPosition -> Beacon
        buildBeacon beaconPosition =
            { beaconPosition = beaconPosition
            , rect =
                { x = beaconData.x
                , y = beaconData.y
                , width = beaconData.width
                , height = beaconData.height
                }
            }
    in
    beaconData
        |> beaconPositionConverter
        |> Maybe.map buildBeacon


beaconPositionConverter : BeaconData -> Maybe BeaconPosition
beaconPositionConverter beaconData =
    case beaconData.id.position of
        "after" ->
            Just <| BeaconPosition.After beaconData.id.identifier

        "before" ->
            Just <| BeaconPosition.Before beaconData.id.identifier

        _ ->
            Nothing
