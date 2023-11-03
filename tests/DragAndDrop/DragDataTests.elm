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
                """{"beaconIdentifier":"an identifier","dragType":"a type","cursor":{"x":1.1,"y":2.2},"beacons":[{"id":{"identifier":"someId","position":"before"},"x":1.1,"y":2.2,"width":3.3,"height":4.4}]}"""
                    |> DecodeHelpers.runDecoder DragData.decoder
                    |> .decoded
                    |> Expect.equal
                        (Ok <|
                            { beaconIdentifier = "an identifier"
                            , beacons =
                                [ { id = { identifier = "someId", position = "before" }
                                  , x = 1.1
                                  , y = 2.2
                                  , width = 3.3
                                  , height = 4.4
                                  }
                                ]
                            , cursor = { x = 1.1, y = 2.2 }
                            , dragType = "a type"
                            }
                        )
        ]
