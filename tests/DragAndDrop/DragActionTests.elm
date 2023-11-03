module DragAndDrop.DragActionTests exposing (suite)

import DragAndDrop.BeaconPosition as BeaconPosition
import DragAndDrop.DragAction as DragAction
import DragAndDrop.DragData as DragData exposing (DragData)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)


suite : Test
suite =
    concat
        [ fromDragData
        ]


fromDragData : Test
fromDragData =
    describe "fromDragData"
        [ test "converts an unknown dragType to NoOp" <|
            \() ->
                { beaconIdentifier = "any identifier"
                , beacons = []
                , cursor = { x = 0, y = 0 }
                , dragType = "unknown"
                }
                    |> DragAction.fromDragData (DragData.TabHeader "1")
                    |> Expect.equal DragAction.NoOp
        , test "converts a 'stop' dragType to Stop" <|
            \() ->
                { beaconIdentifier = "any identifier"
                , beacons = []
                , cursor = { x = 0, y = 0 }
                , dragType = "stop"
                }
                    |> DragAction.fromDragData (DragData.TabHeader "1")
                    |> Expect.equal DragAction.Stop
        , fuzz (Fuzz.pair Fuzz.niceFloat Fuzz.niceFloat) "converts a 'move' dragType to Move (with cursor and beacon details)" <|
            \( fuzzedX, fuzzedY ) ->
                { beaconIdentifier = "any identifier"
                , beacons =
                    [ { id = { identifier = "0", position = "before" }
                      , x = 1.1
                      , y = 2.2
                      , width = 3.3
                      , height = 4.4
                      }
                    , { id = { identifier = "1", position = "after" }
                      , x = 5.5
                      , y = 6.6
                      , width = 7.7
                      , height = 8.8
                      }
                    ]
                , cursor = { x = fuzzedX, y = fuzzedY }
                , dragType = "move"
                }
                    |> DragAction.fromDragData (DragData.TabHeader "1")
                    |> Expect.equal
                        (DragAction.Move
                            { cursor = { x = fuzzedX, y = fuzzedY }
                            , beacons =
                                [ { beaconPosition = BeaconPosition.Before "0"
                                  , rect =
                                        { x = 1.1
                                        , y = 2.2
                                        , width = 3.3
                                        , height = 4.4
                                        }
                                  }
                                , { beaconPosition = BeaconPosition.After "1"
                                  , rect =
                                        { x = 5.5
                                        , y = 6.6
                                        , width = 7.7
                                        , height = 8.8
                                        }
                                  }
                                ]
                            }
                        )
        , test "ignores invalid 'positions'" <|
            \() ->
                { beaconIdentifier = "any identifier"
                , beacons =
                    [ { id = { identifier = "0", position = "xxx" }
                      , x = 1.1
                      , y = 2.2
                      , width = 3.3
                      , height = 4.4
                      }
                    , { id = { identifier = "1", position = "after" }
                      , x = 5.5
                      , y = 6.6
                      , width = 7.7
                      , height = 8.8
                      }
                    ]
                , cursor = { x = 0.0, y = 0.0 }
                , dragType = "move"
                }
                    |> DragAction.fromDragData (DragData.TabHeader "1")
                    |> Expect.equal
                        (DragAction.Move
                            { cursor = { x = 0.0, y = 0.0 }
                            , beacons =
                                [ { beaconPosition = BeaconPosition.After "1"
                                  , rect =
                                        { x = 5.5
                                        , y = 6.6
                                        , width = 7.7
                                        , height = 8.8
                                        }
                                  }
                                ]
                            }
                        )
        ]
