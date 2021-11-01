module CardBoardConfigTests exposing (suite)

import CardBoardConfig
import DateBoard
import Expect
import TagBoard
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
    describe "encoding and decoding config"
        [ test "encodes config correctly" <|
            \() ->
                defaultConfig
                    |> TsEncode.runExample CardBoardConfig.configsEncoder
                    |> .output
                    |> Expect.equal """{"boardConfigs":[{"tag":"tagBoardConfig","data":{"columns":[],"completedCount":10,"includeOthers":false,"includeUntagged":false,"title":""}}]}"""
        , test "produces the expected type" <|
            \() ->
                defaultConfig
                    |> TsEncode.runExample CardBoardConfig.configsEncoder
                    |> .tsType
                    |> Expect.equal """{ boardConfigs : ({ data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" } | { data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" })[] }"""

        -- TODO : Add this test case when I have made the sig for defaultConfig : CardBoardConfig.CardBoardConfig
        --        I can also then add the various property tests!
        --
        --
        -- , test "can decode the encoded string back to the original" <|
        --     \() ->
        --         defaultConfig
        --             |> TsEncode.runExample CardBoardConfig.configsEncoder
        --             |> .output
        --             |> runDecoder CardBoardConfig.decoder
        --             |> .decoded
        --             |> Expect.equal (Ok defaultConfig)
        ]



-- HELPERS


defaultConfig : { boardConfigs : List CardBoardConfig.Config }
defaultConfig =
    { boardConfigs = [ CardBoardConfig.TagBoardConfig TagBoard.defaultConfig ] }


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
