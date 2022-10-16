module CardBoardSettings exposing
    ( Settings
    , boardConfigs
    , currentVersion
    , decoder
    , encoder
    , globalSettings
    )

import BoardConfig exposing (BoardConfig)
import GlobalSettings exposing (GlobalSettings)
import Json.Encode as JE
import Semver
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Settings =
    { boardConfigs : List BoardConfig
    , globalSettings : GlobalSettings
    , version : Semver.Version
    }



-- UTILITIES


boardConfigs : Settings -> List BoardConfig
boardConfigs =
    .boardConfigs


currentVersion : Semver.Version
currentVersion =
    Semver.version 0 5 0 [] []


globalSettings : Settings -> GlobalSettings
globalSettings =
    .globalSettings



-- SERIALIZE


encoder : TsEncode.Encoder Settings
encoder =
    TsEncode.object
        [ TsEncode.required "version" .version semverEncoder
        , TsEncode.required "data" identity dataEncoder
        ]


decoder : TsDecode.Decoder Settings
decoder =
    TsDecode.field "version" TsDecode.string
        |> TsDecode.andThen versionedSettingsDecoder



-- PRIVATE


dataEncoder : TsEncode.Encoder { a | boardConfigs : List BoardConfig, globalSettings : GlobalSettings }
dataEncoder =
    TsEncode.object
        [ TsEncode.required "boardConfigs" .boardConfigs (TsEncode.list BoardConfig.encoder)
        , TsEncode.required "globalSettings" .globalSettings GlobalSettings.encoder
        ]


semverEncoder : TsEncode.Encoder Semver.Version
semverEncoder =
    TsEncode.string
        |> TsEncode.map Semver.print


versionedSettingsDecoder : TsDecode.AndThenContinuation (String -> TsDecode.Decoder Settings)
versionedSettingsDecoder =
    TsDecode.andThenInit
        (\v_0_5_0 v_0_4_0 v_0_3_0 v_0_2_0 v_0_1_0 unsupportedVersion version_ ->
            case version_ of
                "0.5.0" ->
                    v_0_5_0

                "0.4.0" ->
                    v_0_4_0

                "0.3.0" ->
                    v_0_3_0

                "0.2.0" ->
                    v_0_2_0

                "0.1.0" ->
                    v_0_1_0

                _ ->
                    unsupportedVersion
        )
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_5_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_4_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_3_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_2_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_1_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" unsupportedVersionDecoder)


v_0_5_0_Decoder : TsDecode.Decoder Settings
v_0_5_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder_v_0_5_0))
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_4_0_Decoder : TsDecode.Decoder Settings
v_0_4_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder_v_0_4_0))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_3_0_Decoder : TsDecode.Decoder Settings
v_0_3_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder_v_0_3_0))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_2_0_Decoder : TsDecode.Decoder Settings
v_0_2_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder_v_0_2_0))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_1_0_Decoder : TsDecode.Decoder Settings
v_0_1_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder_v_0_1_0))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


unsupportedVersionDecoder : TsDecode.Decoder Settings
unsupportedVersionDecoder =
    TsDecode.fail "Unsupported settings file version"
