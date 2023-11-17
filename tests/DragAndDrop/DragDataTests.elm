module DragAndDrop.DragDataTests exposing (suite)

import DragAndDrop.BeaconPosition as BeaconPosition
import DragAndDrop.DragData as DragData
import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes valid input" <|
            \() ->
                """{"beaconType":"an identifier","dragAction":"move","cursor":{"x":1.1,"y":2.2},"offset":{"x":3.3,"y":4.4},"draggedNodeRect":{"x":1.2,"y":3.4,"width":3.33,"height":4.44},"beacons":[{"beaconPosition":{"uniqueId":"someId","position":"before"},"rect":{"x":1.1,"y":2.2,"width":3.3,"height":4.4}}]}"""
                    |> DecodeHelpers.runDecoder DragData.decoder
                    |> .decoded
                    |> Expect.equal
                        (Ok <|
                            { beaconType = "an identifier"
                            , beacons =
                                [ { beaconPosition = BeaconPosition.Before "someId"
                                  , rect = { x = 1.1, y = 2.2, width = 3.3, height = 4.4 }
                                  }
                                ]
                            , cursor = { x = 1.1, y = 2.2 }
                            , offset = { x = 3.3, y = 4.4 }
                            , draggedNodeRect = { x = 1.2, y = 3.4, width = 3.33, height = 4.44 }
                            , dragAction = DragData.Move
                            }
                        )
        ]
