module DragAndDrop.BeaconPositionTests exposing (suite)

import DragAndDrop.BeaconPosition
import Expect
import Test exposing (..)


suite : Test
suite =
    concat
        [ encoder
        ]


encoder : Test
encoder =
    describe "decoder"
        [ test "is Ture" <|
            \() ->
                True
                    |> Expect.equal True
        ]
