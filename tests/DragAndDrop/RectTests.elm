module DragAndDrop.RectTests exposing (suite)

import DragAndDrop.Rect as Rect
import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Test exposing (..)


suite : Test
suite =
    concat
        [ centre
        , closestTo
        , decoder
        ]


centre : Test
centre =
    describe "centre"
        [ test "returns the point halfway along the x and y axis" <|
            \() ->
                { x = 1.1
                , y = 2.2
                , width = 10
                , height = 20
                }
                    |> Rect.centre
                    |> Expect.equal
                        { x = 6.1
                        , y = 12.2
                        }
        ]


closestTo : Test
closestTo =
    describe "closestTo"
        [ test "returns Nothing if there are no rects" <|
            \() ->
                []
                    |> Rect.closestTo { x = 0, y = 0 }
                    |> Expect.equal Nothing
        , test "returns the rect if there is only one" <|
            \() ->
                [ { id = "1", rect = { x = 0, y = 0, width = 1, height = 1 } }
                ]
                    |> Rect.closestTo { x = 0, y = 0 }
                    |> Expect.equal (Just "1")
        , test "returns the first of the two if there are more than one at the same distance" <|
            \() ->
                [ { id = "1", rect = { x = 1, y = 1, width = 1, height = 1 } }
                , { id = "2", rect = { x = 0, y = 0, width = 1, height = 1 } }
                , { id = "3", rect = { x = 0, y = 0, width = 1, height = 1 } }
                ]
                    |> Rect.closestTo { x = 0, y = 0 }
                    |> Expect.equal (Just "2")
        , test "returns the closest of the bunch" <|
            \() ->
                [ { id = "1", rect = { x = 1, y = 1, width = 1, height = 1 } }
                , { id = "2", rect = { x = 10, y = 30, width = 100, height = 100 } }
                , { id = "3", rect = { x = 10, y = 30, width = 10, height = 10 } }
                , { id = "4", rect = { x = 100, y = 0, width = 1, height = 1 } }
                ]
                    |> Rect.closestTo { x = 12, y = 20 }
                    |> Expect.equal (Just "3")
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
