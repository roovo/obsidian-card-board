module CardBoardSettings exposing
    ( GlobalSettings
    , Settings
    , boardConfigs
    , currentVersion
    , decoder
    , defaultGlobalSettings
    , encoder
    , globalSettings
    )

import BoardConfig exposing (BoardConfig)
import Semver
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode


currentVersion : Semver.Version
currentVersion =
    Semver.version 0 2 0 [] []



-- TYPES


type alias Settings =
    { boardConfigs : List BoardConfig
    , globalSettings : GlobalSettings
    , version : Semver.Version
    }


type alias GlobalSettings =
    { hideCompletedSubtasks : Bool
    , ignorePaths : String
    , subTaskDisplayLimit : Maybe Int
    }


defaultGlobalSettings : GlobalSettings
defaultGlobalSettings =
    { hideCompletedSubtasks = False
    , ignorePaths = ""
    , subTaskDisplayLimit = Nothing
    }



-- INFO


boardConfigs : Settings -> List BoardConfig
boardConfigs =
    .boardConfigs


globalSettings : Settings -> GlobalSettings
globalSettings =
    .globalSettings



-- SERIALIZATION


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



-- HELPERS - ENCODERS


dataEncoder : TsEncode.Encoder { a | boardConfigs : List BoardConfig, globalSettings : GlobalSettings }
dataEncoder =
    TsEncode.object
        [ TsEncode.required "boardConfigs" .boardConfigs (TsEncode.list BoardConfig.encoder)
        , TsEncode.required "globalSettings" .globalSettings globalSettingsEncoder
        ]


globalSettingsEncoder : TsEncode.Encoder GlobalSettings
globalSettingsEncoder =
    TsEncode.object
        [ TsEncode.required "hideCompletedSubtasks" .hideCompletedSubtasks TsEncode.bool
        , TsEncode.required "ignorePaths" .ignorePaths TsEncode.string
        , TsEncode.required "subTaskDisplayLimit" .subTaskDisplayLimit (TsEncode.maybe TsEncode.int)
        ]


semverEncoder : TsEncode.Encoder Semver.Version
semverEncoder =
    TsEncode.string
        |> TsEncode.map Semver.print



-- HELPERS - DECODERS


versionedSettingsDecoder : TsDecode.AndThenContinuation (String -> TsDecode.Decoder Settings)
versionedSettingsDecoder =
    TsDecode.andThenInit
        (\current v_0_1_0 unsupportedVersion version_ ->
            case version_ of
                "0.2.0" ->
                    current

                "0.1.0" ->
                    v_0_1_0

                _ ->
                    unsupportedVersion
        )
        |> TsDecode.andThenDecoder (TsDecode.field "data" currentVersionDecoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_1_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" unsupportedVersionDecoder)


currentVersionDecoder : TsDecode.Decoder Settings
currentVersionDecoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder))
        |> TsDecode.andMap (TsDecode.field "globalSettings" globalSettingsDecoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_1_0_Decoder : TsDecode.Decoder Settings
v_0_1_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.list BoardConfig.decoder_v_0_1_0))
        |> TsDecode.andMap (TsDecode.succeed defaultGlobalSettings)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


unsupportedVersionDecoder : TsDecode.Decoder Settings
unsupportedVersionDecoder =
    TsDecode.fail "Unsupported settings file version"


globalSettingsDecoder : TsDecode.Decoder GlobalSettings
globalSettingsDecoder =
    TsDecode.succeed GlobalSettings
        |> TsDecode.andMap (TsDecode.field "hideCompletedSubtasks" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "ignorePaths" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "subTaskDisplayLimit" (TsDecode.maybe TsDecode.int))


defaultGlobalSettingsDecoder : TsDecode.Decoder GlobalSettings
defaultGlobalSettingsDecoder =
    TsDecode.succeed defaultGlobalSettings
