module EncodeHelpersTests exposing (suite)

import Date
import EncodeHelpers
import Expect
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ encoder
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes the date as a RataDie integer" <|
            \() ->
                Date.fromRataDie 123456
                    |> TsEncode.runExample EncodeHelpers.dateEncoder
                    |> .output
                    |> Expect.equal "123456"
        ]
