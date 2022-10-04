module CardBoardSettingsTests exposing (suite)

import BoardConfig
import CardBoardSettings
import Expect
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Semver
import Test exposing (..)
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
        [ test "is 0.3.0" <|
            \() ->
                CardBoardSettings.currentVersion
                    |> Semver.print
                    |> Expect.equal "0.3.0"
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding settings"
        [ test "can decode the encoded string back to the original" <|
            \() ->
                exampleSettings
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Expect.equal (Ok exampleSettings)
        , test "fails if the version number is unsupported" <|
            \() ->
                { exampleSettings | version = Semver.version 0 0 0 [] [] }
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing

        -- , test "builds the correct tsType" <|
        --     \() ->
        --         exampleSettings
        --             |> TsEncode.runExample CardBoardSettings.encoder
        --             |> .tsType
        --             |> Expect.equal ""
        ]



-- HELPERS


exampleSettings : CardBoardSettings.Settings
exampleSettings =
    { boardConfigs =
        [ BoardConfig.TagBoardConfig BoardConfigHelpers.exampleTagBoardConfig
        , BoardConfig.DateBoardConfig BoardConfigHelpers.exampleDateBoardConfig
        ]
    , version = CardBoardSettings.currentVersion
    }
