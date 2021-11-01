module CardBoardSettingsTests exposing (suite)

import CardBoardSettings
import Expect
import Semver
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
    describe "encoding and decoding settings"
        [ test "encodes settings correctly" <|
            \() ->
                defaultSettings
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> Expect.equal """{"version":"0.1.0","data":{"boardConfigs":[]}}"""
        , test "produces the expected type" <|
            \() ->
                defaultSettings
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .tsType
                    |> Expect.equal
                        ("{ data : { boardConfigs : ("
                            ++ "{ data : { columns : { displayTitle : string; tag : string }[]; completedCount : number; includeOthers : boolean; includeUntagged : boolean; title : string }; tag : \"tagBoardConfig\" } | "
                            ++ "{ data : { completedCount : number; includeUndated : boolean; title : string }; tag : \"dateBoardConfig\" }"
                            ++ ")[] }; "
                            ++ "version : string }"
                        )
        , test "can decode the encoded string back to the original" <|
            \() ->
                defaultSettings
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Expect.equal (Ok defaultSettings)
        , test "fails if the version number is unsupported" <|
            \() ->
                { defaultSettings | version = Semver.version 0 0 0 [] [] }
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]



-- HELPERS


defaultSettings : CardBoardSettings.Settings
defaultSettings =
    { boardConfigs = []
    , version = Semver.version 0 1 0 [] []
    }


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
