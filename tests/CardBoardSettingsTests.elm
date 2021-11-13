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
        [ currentVersion
        , encodeDecode
        ]


currentVersion : Test
currentVersion =
    describe "currentVersion"
        [ test "is 0.2.0" <|
            \() ->
                CardBoardSettings.currentVersion
                    |> Semver.print
                    |> Expect.equal "0.2.0"
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding settings"
        [ test "can decode the encoded string back to the original" <|
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
    , globalSettings = defaultGlobalSettings
    , version = Semver.version 0 2 0 [] []
    }


defaultGlobalSettings : CardBoardSettings.GlobalSettings
defaultGlobalSettings =
    { hideCompletedSubtasks = False
    , ignorePaths = ""
    , subTaskDisplayLimit = Nothing
    }


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
