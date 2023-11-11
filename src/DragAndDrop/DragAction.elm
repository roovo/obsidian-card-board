module DragAndDrop.DragAction exposing
    ( DragAction(..)
    , fromDragData
    )

import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords exposing (Coords)
import DragAndDrop.DragData exposing (BeaconData, DragData)
import DragAndDrop.Rect as Rect exposing (Rect)
import Maybe.Extra as ME
import TsJson.Decode as TsDecode



-- TYPES


type DragAction
    = Move Positions
    | Stop
    | NoOp


type alias Positions =
    { cursor : Coords
    , beacons : List BeaconData
    }



-- CONVERTERS


fromDragData : DragData -> DragAction
fromDragData dragData =
    case dragData.dragType of
        "stop" ->
            Stop

        "move" ->
            Move
                { cursor = dragData.cursor
                , beacons = dragData.beacons
                }

        _ ->
            NoOp
