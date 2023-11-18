module DragAndDrop.DragTracker exposing
    ( ClientData
    , DragTracker(..)
    , dragType
    , init
    , isDragging
    , moveDragable
    , stopTracking
    , waitForDrag
    )

import DragAndDrop.Coords as Coords exposing (Coords)
import DragAndDrop.DragData exposing (DragData)
import DragAndDrop.Rect as Rect exposing (Rect)



-- TYPES


type DragTracker
    = NotDragging
    | Waiting ClientData
    | Dragging ClientData DomData


type alias ClientData =
    { uniqueId : String
    , clientPos : Coords
    , offsetPos : Coords
    }


type alias DomData =
    { dragType : String
    , offset : Coords
    , draggedNodeStartRect : Rect
    }


init : DragTracker
init =
    NotDragging



-- INFO


dragType : DragTracker -> Maybe String
dragType dragTracker =
    case dragTracker of
        NotDragging ->
            Nothing

        Waiting _ ->
            Nothing

        Dragging _ domData ->
            Just domData.dragType


isDragging : DragTracker -> Bool
isDragging dragTracker =
    case dragTracker of
        NotDragging ->
            False

        Waiting _ ->
            False

        Dragging _ _ ->
            True



-- STATE TRANSFORMS


moveDragable : DragData -> DragTracker -> DragTracker
moveDragable dragData dragTracker =
    case dragTracker of
        NotDragging ->
            dragTracker

        Waiting clientData ->
            Dragging
                { clientData | clientPos = dragData.cursor }
                { offset = dragData.offset
                , draggedNodeStartRect = dragData.draggedNodeRect
                , dragType = dragData.dragType
                }

        Dragging clientData domData ->
            Dragging
                { clientData | clientPos = dragData.cursor }
                { domData
                    | offset = dragData.offset
                    , dragType = dragData.dragType
                }


stopTracking : DragTracker
stopTracking =
    NotDragging


waitForDrag : ClientData -> DragTracker
waitForDrag clientData =
    Waiting clientData
