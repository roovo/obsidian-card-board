module BoardConfigTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import Expect
import TagBoard
import Test exposing (..)
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ default
        , encodeDecode
        ]


default : Test
default =
    describe "default"
        [ test "is for a TagBoard" <|
            \() ->
                BoardConfig.default
                    |> BoardConfig.isForTagBoard
                    |> Expect.equal True
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "encodes config correctly" <|
            \() ->
                defaultConfig
                    |> TsEncode.runExample BoardConfig.encoder
                    |> .output
                    |> Expect.equal """{"tag":"tagBoardConfig","data":{"columns":[],"completedCount":10,"includeOthers":false,"includeUntagged":false,"title":""}}"""
        , test "produces the expected type" <|
            \() ->
                defaultConfig
                    |> TsEncode.runExample BoardConfig.encoder
                    |> .tsType
                    |> Expect.equal """{ data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : "tagBoardConfig" } | { data : { completedCount : number; includeUndated : boolean; title : string }; tag : "dateBoardConfig" }"""
        , test "can decode the encoded string back to the original" <|
            \() ->
                defaultConfig
                    |> TsEncode.runExample BoardConfig.encoder
                    |> .output
                    |> runDecoder BoardConfig.decoder
                    |> .decoded
                    |> Expect.equal (Ok defaultConfig)
        ]



-- HELPERS


defaultConfig : BoardConfig
defaultConfig =
    BoardConfig.TagBoardConfig TagBoard.defaultConfig


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
