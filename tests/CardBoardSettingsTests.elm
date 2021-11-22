module CardBoardSettingsTests exposing (suite)

import BoardConfig
import CardBoardSettings
import Expect
import Filter
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
                exampleSettings
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Expect.equal (Ok exampleSettings)
        , test "fails if the version number is unsupported" <|
            \() ->
                { exampleSettings | version = Semver.version 0 0 0 [] [] }
                    |> TsEncode.runExample CardBoardSettings.encoder
                    |> .output
                    |> runDecoder CardBoardSettings.decoder
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]



-- HELPERS


exampleSettings : CardBoardSettings.Settings
exampleSettings =
    { boardConfigs =
        [ BoardConfig.TagBoardConfig
            { columns = [ { tag = "foo", displayTitle = "bar" } ]
            , completedCount = 10
            , filters = [ Filter.PathFilter "path1", Filter.PathFilter "path2", Filter.TagFilter "aTag", Filter.TagFilter "anotherTag" ]
            , includeOthers = False
            , includeUntagged = True
            , title = "A tagboard"
            }
        , BoardConfig.DateBoardConfig
            { completedCount = 15
            , filters = [ Filter.PathFilter "date/path", Filter.TagFilter "dateTag" ]
            , includeUndated = False
            , title = "A dateboard"
            }
        ]
    , globalSettings = exampleGlobalSettings
    , version = Semver.version 0 2 0 [] []
    }


exampleGlobalSettings : CardBoardSettings.GlobalSettings
exampleGlobalSettings =
    { hideCompletedSubtasks = True
    , ignorePaths = "a/path"
    , subTaskDisplayLimit = Just 17
    }


type alias DecodeResult value =
    { decoded : Result String value
    , tsType : String
    }


runDecoder : TsDecode.Decoder value -> String -> DecodeResult value
runDecoder decoder input =
    TsDecode.runExample input decoder
