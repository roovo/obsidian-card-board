module DragAndDrop.RectTests exposing (suite)

import DragAndDrop.Rect as Rect
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
                """{"x":1.1,"y":2.2,"width":3.3,"height":4.4}"""
                    |> DecodeHelpers.runDecoder Rect.decoder
                    |> .decoded
                    |> Expect.equal
                        (Ok <|
                            { x = 1.1
                            , y = 2.2
                            , width = 3.3
                            , height = 4.4
                            }
                        )
        ]
