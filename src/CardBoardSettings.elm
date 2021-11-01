module CardBoardSettings exposing
    ( Settings
    , decoder
    , encoder
    , version
    )

import BoardConfig exposing (BoardConfig)
import Semver
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Settings =
    { boardConfigs : List BoardConfig
    , version : Semver.Version
    }



-- INFO


version : Settings -> String
version settings =
    Semver.print settings.version



-- SERIALIZATION


encoder : TsEncode.Encoder Settings
encoder =
    TsEncode.object
        [ TsEncode.required "version" .version semverEncoder
        , TsEncode.required "data" .boardConfigs boardConfigsEncoder
        ]


decoder : TsDecode.Decoder Settings
decoder =
    TsDecode.field "version" TsDecode.string
        |> TsDecode.andThen versionedSettingsDecoder



-- HELPERS


semverEncoder : TsEncode.Encoder Semver.Version
semverEncoder =
    TsEncode.string
        |> TsEncode.map Semver.print


boardConfigsEncoder : TsEncode.Encoder (List BoardConfig)
boardConfigsEncoder =
    TsEncode.object
        [ TsEncode.required "boardConfigs" identity (TsEncode.list BoardConfig.encoder)
        ]


versionedSettingsDecoder : TsDecode.AndThenContinuation (String -> TsDecode.Decoder Settings)
versionedSettingsDecoder =
    TsDecode.andThenInit
        (\v0_1_0 unsupportedVersion version_ ->
            case version_ of
                "0.1.0" ->
                    v0_1_0

                _ ->
                    unsupportedVersion
        )
        |> TsDecode.andThenDecoder (TsDecode.field "data" v0_1_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" unsupportedVersionDecoder)


v0_1_0_Decoder : TsDecode.Decoder Settings
v0_1_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder))
        |> TsDecode.andMap (TsDecode.succeed (Semver.version 0 1 0 [] []))


unsupportedVersionDecoder : TsDecode.Decoder Settings
unsupportedVersionDecoder =
    TsDecode.fail "Unsupported settings file version"
