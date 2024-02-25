module DecodeHelpersTests exposing (suite)

import Date
import DecodeHelpers
import Expect
import Helpers.DecodeHelpers as DecodeTestHelpers
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes a RataDie integer as a date" <|
            \() ->
                "123456"
                    |> DecodeTestHelpers.runDecoder DecodeHelpers.dateDecoder
                    |> .decoded
                    |> Expect.equal (Ok <| Date.fromRataDie 123456)
        ]
