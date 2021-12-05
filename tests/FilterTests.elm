module FilterTests exposing (suite)

import Expect
import Filter exposing (Filter)
import Helpers.DecodeHelpers as DecodeHelpers
import Parser
import Test exposing (..)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ encodeDecode
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding filters"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleFilters
                    |> TsEncode.runExample (TsEncode.list Filter.encoder)
                    |> .output
                    |> DecodeHelpers.runDecoder (TsDecode.list Filter.decoder)
                    |> .decoded
                    |> Expect.equal (Ok exampleFilters)
        ]



-- HELPERS


exampleFilters : List Filter
exampleFilters =
    [ Filter.PathFilter "a/path"
    , Filter.TagFilter "a_tag"
    ]
