module DragAndDrop.DragTrackerTests exposing (suite)

import DragAndDrop.DragData as DragData
import DragAndDrop.DragTracker as DragTracker
import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ init
        , isDragging
        , moveDragable
        , stopTracking
        , waitForDrag
        ]


init : Test
init =
    describe "init"
        [ test "is in the NotDragging state" <|
            \() ->
                DragTracker.init
                    |> Expect.equal DragTracker.NotDragging
        , test "initialises to not dragging" <|
            \() ->
                DragTracker.init
                    |> DragTracker.isDragging
                    |> Expect.equal False
        ]


isDragging : Test
isDragging =
    describe "isDragging"
        [ test "is False if in NotDragging state" <|
            \() ->
                DragTracker.init
                    |> DragTracker.isDragging
                    |> Expect.equal False
        , test "returns False if it was Waiting" <|
            \() ->
                DragTracker.waitForDrag
                    { uniqueId = "an id"
                    , clientPos = { x = 0, y = 1 }
                    , offsetPos = { x = 1, y = 2 }
                    }
                    |> DragTracker.isDragging
                    |> Expect.equal False
        , test "returns True if Dragging" <|
            \() ->
                DragTracker.Dragging
                    { uniqueId = "an id"
                    , clientPos = { x = 0, y = 0 }
                    , offsetPos = { x = 1, y = 2 }
                    }
                    { offset = { x = 0, y = 0 }
                    , draggedNodeStartRect = { x = 0, y = 0, width = 0, height = 0 }
                    }
                    |> DragTracker.isDragging
                    |> Expect.equal True
        ]


moveDragable : Test
moveDragable =
    describe "moveDragable"
        [ test "does nothing if in NotDragging state" <|
            \() ->
                let
                    dragData =
                        { beaconType = ""
                        , dragAction = DragData.Move
                        , cursor = { x = 0, y = 1 }
                        , offset = { x = 1, y = 2 }
                        , draggedNodeRect = { x = 2, y = 3, width = 4, height = 5 }
                        , beacons = []
                        }
                in
                DragTracker.init
                    |> DragTracker.moveDragable dragData
                    |> Expect.equal DragTracker.NotDragging
        , test "moves into the Dragging state if it was Waiting" <|
            \() ->
                let
                    dragData =
                        { beaconType = ""
                        , dragAction = DragData.Move
                        , cursor = { x = 1.1, y = 2.2 }
                        , offset = { x = 1, y = 2 }
                        , draggedNodeRect = { x = 2, y = 3, width = 4, height = 5 }
                        , beacons = []
                        }

                    clientData =
                        { uniqueId = "an id"
                        , clientPos = { x = 0, y = 1 }
                        , offsetPos = { x = 1, y = 2 }
                        }
                in
                DragTracker.waitForDrag clientData
                    |> DragTracker.moveDragable dragData
                    |> Expect.equal
                        (DragTracker.Dragging
                            { uniqueId = "an id"
                            , clientPos = { x = 1.1, y = 2.2 }
                            , offsetPos = { x = 1, y = 2 }
                            }
                            { offset = { x = 1, y = 2 }
                            , draggedNodeStartRect = { x = 2, y = 3, width = 4, height = 5 }
                            }
                        )
        , test "updates clientPos and offset if it was already Dragging" <|
            \() ->
                let
                    dragData =
                        { beaconType = ""
                        , dragAction = DragData.Move
                        , cursor = { x = 1.1, y = 2.2 }
                        , offset = { x = 1, y = 2 }
                        , draggedNodeRect = { x = 2, y = 3, width = 4, height = 5 }
                        , beacons = []
                        }

                    clientData =
                        { uniqueId = "an id"
                        , clientPos = { x = 0, y = 1 }
                        , offsetPos = { x = 1, y = 2 }
                        }
                in
                DragTracker.Dragging
                    { uniqueId = "an id"
                    , clientPos = { x = 0, y = 0 }
                    , offsetPos = { x = 1, y = 2 }
                    }
                    { offset = { x = 0, y = 0 }
                    , draggedNodeStartRect = { x = 0, y = 0, width = 0, height = 0 }
                    }
                    |> DragTracker.moveDragable dragData
                    |> Expect.equal
                        (DragTracker.Dragging
                            { uniqueId = "an id"
                            , clientPos = { x = 1.1, y = 2.2 }
                            , offsetPos = { x = 1, y = 2 }
                            }
                            { offset = { x = 1, y = 2 }
                            , draggedNodeStartRect = { x = 0, y = 0, width = 0, height = 0 }
                            }
                        )
        ]


stopTracking : Test
stopTracking =
    describe "stopTracking"
        [ test "returns the NotDragging state" <|
            \() ->
                DragTracker.stopTracking
                    |> Expect.equal DragTracker.NotDragging
        ]


waitForDrag : Test
waitForDrag =
    describe "waitForDrag"
        [ test "is in the waiting state" <|
            \() ->
                let
                    clientData =
                        { uniqueId = "an id"
                        , clientPos = { x = 0, y = 1 }
                        , offsetPos = { x = 1, y = 2 }
                        }
                in
                DragTracker.waitForDrag clientData
                    |> Expect.equal (DragTracker.Waiting clientData)
        , test "it is not dragging" <|
            \() ->
                let
                    clientData =
                        { uniqueId = "an id"
                        , clientPos = { x = 0, y = 1 }
                        , offsetPos = { x = 1, y = 2 }
                        }
                in
                DragTracker.waitForDrag clientData
                    |> DragTracker.isDragging
                    |> Expect.equal False
        ]
