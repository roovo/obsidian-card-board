module Settings exposing
    ( Settings
    , boardConfigs
    , cleanupNames
    , currentVersion
    , decoder
    , default
    , defaultColumnNames
    , encoder
    , globalSettings
    , moveBoard
    , restrictSpecialColumns
    , switchToBoard
    , updateCurrentBoard
    , updatePath
    )

import BoardConfig exposing (BoardConfig)
import Column
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Filter
import GlobalSettings exposing (GlobalSettings)
import SafeZipper exposing (SafeZipper)
import Semver
import TsJson.Decode as TsDecode
import TsJson.Encode as TsEncode



-- TYPES


type alias Settings =
    { boardConfigs : SafeZipper BoardConfig
    , globalSettings : GlobalSettings
    , version : Semver.Version
    }


default : Settings
default =
    { boardConfigs = SafeZipper.empty
    , globalSettings = GlobalSettings.default
    , version = currentVersion
    }



-- UTILITIES


boardConfigs : Settings -> SafeZipper BoardConfig
boardConfigs =
    .boardConfigs


cleanupNames : Settings -> Settings
cleanupNames settings =
    case SafeZipper.currentIndex settings.boardConfigs of
        Just index ->
            settings
                |> replaceMissingBoardNames
                |> enforceUniqueBoardNames
                |> cleanupColumnNames (defaultColumnNames settings)
                |> boardConfigs
                |> SafeZipper.atIndex index
                |> (\cs -> { settings | boardConfigs = cs })

        Nothing ->
            settings


defaultColumnNames : Settings -> DefaultColumnNames
defaultColumnNames =
    .defaultColumnNames << globalSettings


currentVersion : Semver.Version
currentVersion =
    Semver.version 0 11 0 [] []


globalSettings : Settings -> GlobalSettings
globalSettings =
    .globalSettings



-- TRANSFORM


moveBoard : String -> BeaconPosition -> Settings -> Settings
moveBoard draggedId beaconPosition settings =
    let
        movedBoardConfigs : SafeZipper BoardConfig
        movedBoardConfigs =
            settings
                |> boardConfigs
                |> SafeZipper.toList
                |> BeaconPosition.performMove draggedId beaconPosition BoardConfig.name
                |> SafeZipper.fromList

        movedBoardIndex : Int
        movedBoardIndex =
            movedBoardConfigs
                |> SafeZipper.findIndex (\c -> BoardConfig.name c == draggedId)
                |> Maybe.withDefault 0
    in
    { settings | boardConfigs = SafeZipper.atIndex movedBoardIndex movedBoardConfigs }


switchToBoard : Int -> Settings -> Settings
switchToBoard index settings =
    { settings | boardConfigs = SafeZipper.atIndex index settings.boardConfigs }


updateCurrentBoard : (BoardConfig -> BoardConfig) -> Settings -> Settings
updateCurrentBoard fn settings =
    { settings | boardConfigs = SafeZipper.updateCurrent fn settings.boardConfigs }


updatePath : String -> String -> Settings -> Settings
updatePath oldPath newPath settings =
    let
        updateFilters : BoardConfig -> BoardConfig
        updateFilters config =
            BoardConfig.mapFilters (Filter.updatePath oldPath newPath) config
    in
    { settings | boardConfigs = SafeZipper.map updateFilters settings.boardConfigs }



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
        |> TsDecode.map restrictSpecialColumns
        |> TsDecode.map cleanupNames



-- PRIVATE


dataEncoder : TsEncode.Encoder { a | boardConfigs : SafeZipper BoardConfig, globalSettings : GlobalSettings }
dataEncoder =
    TsEncode.object
        [ TsEncode.required "boardConfigs" .boardConfigs (TsEncode.map SafeZipper.toList <| TsEncode.list BoardConfig.encoder)
        , TsEncode.required "globalSettings" .globalSettings GlobalSettings.encoder
        ]


enforceUniqueBoardNames : Settings -> Settings
enforceUniqueBoardNames settings =
    let
        helper : Int -> BoardConfig -> ( List String, SafeZipper BoardConfig ) -> ( List String, SafeZipper BoardConfig )
        helper index config ( names, cs ) =
            let
                uniqueName : String
                uniqueName =
                    if List.member (String.replace " " "_" <| BoardConfig.name config) names then
                        BoardConfig.name config ++ "." ++ String.fromInt index

                    else
                        BoardConfig.name config

                safeConfig : BoardConfig
                safeConfig =
                    BoardConfig.updateName uniqueName config

                ts : List String
                ts =
                    String.replace " " "_" uniqueName :: names
            in
            ( ts, SafeZipper.add safeConfig cs )

        withUniqueBoardNames : SafeZipper BoardConfig
        withUniqueBoardNames =
            SafeZipper.indexedFoldl helper ( [], SafeZipper.empty ) settings.boardConfigs
                |> Tuple.second
    in
    { settings | boardConfigs = withUniqueBoardNames }


replaceMissingBoardNames : Settings -> Settings
replaceMissingBoardNames settings =
    let
        withNoMissingNames : BoardConfig -> BoardConfig
        withNoMissingNames config =
            if String.isEmpty (String.trim <| BoardConfig.name config) then
                BoardConfig.updateName "Unnamed" config

            else
                config
    in
    { settings | boardConfigs = SafeZipper.map withNoMissingNames settings.boardConfigs }


cleanupColumnNames : DefaultColumnNames -> Settings -> Settings
cleanupColumnNames defaultColumnNames_ settings =
    { settings
        | boardConfigs =
            SafeZipper.map
                (BoardConfig.cleanupColumnNames defaultColumnNames_)
                settings.boardConfigs
    }


restrictSpecialColumns : Settings -> Settings
restrictSpecialColumns settings =
    { settings
        | boardConfigs =
            SafeZipper.map
                BoardConfig.restrictSpecialColumns
                settings.boardConfigs
    }


semverEncoder : TsEncode.Encoder Semver.Version
semverEncoder =
    TsEncode.string
        |> TsEncode.map Semver.print


versionedSettingsDecoder : TsDecode.AndThenContinuation (String -> TsDecode.Decoder Settings)
versionedSettingsDecoder =
    TsDecode.andThenInit
        (\v_0_11_0 v_0_10_0 v_0_9_0 v_0_8_0 v_0_7_0 v_0_6_0 v_0_5_0 v_0_4_0 v_0_3_0 v_0_2_0 v_0_1_0 unsupportedVersion version_ ->
            case version_ of
                "0.11.0" ->
                    v_0_11_0

                "0.10.0" ->
                    v_0_10_0

                "0.9.0" ->
                    v_0_9_0

                "0.8.0" ->
                    v_0_8_0

                "0.7.0" ->
                    v_0_7_0

                "0.6.0" ->
                    v_0_6_0

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
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_11_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_10_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_9_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_8_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_7_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_6_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_5_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_4_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_3_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_2_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" v_0_1_0_Decoder)
        |> TsDecode.andThenDecoder (TsDecode.field "data" unsupportedVersionDecoder)


v_0_11_0_Decoder : TsDecode.Decoder Settings
v_0_11_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_11_0))
            )
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_11_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_10_0_Decoder : TsDecode.Decoder Settings
v_0_10_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_10_0))
            )
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_10_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_9_0_Decoder : TsDecode.Decoder Settings
v_0_9_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_9_0))
            )
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_9_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_8_0_Decoder : TsDecode.Decoder Settings
v_0_8_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_8_0))
            )
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_8_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_7_0_Decoder : TsDecode.Decoder Settings
v_0_7_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_7_0))
            )
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_7_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_6_0_Decoder : TsDecode.Decoder Settings
v_0_6_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_6_0))
            )
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_6_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_5_0_Decoder : TsDecode.Decoder Settings
v_0_5_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_5_0))
            )
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_5_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_4_0_Decoder : TsDecode.Decoder Settings
v_0_4_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_4_0))
            )
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_3_0_Decoder : TsDecode.Decoder Settings
v_0_3_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_3_0))
            )
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_2_0_Decoder : TsDecode.Decoder Settings
v_0_2_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_2_0))
            )
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


v_0_1_0_Decoder : TsDecode.Decoder Settings
v_0_1_0_Decoder =
    (TsDecode.succeed Settings
        |> TsDecode.andMap
            (TsDecode.field "boardConfigs"
                (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_1_0))
            )
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)
    )
        |> TsDecode.map setNamesToDefault


unsupportedVersionDecoder : TsDecode.Decoder Settings
unsupportedVersionDecoder =
    TsDecode.fail "Unsupported settings file version"


setNamesToDefault : Settings -> Settings
setNamesToDefault settings =
    { settings
        | boardConfigs =
            SafeZipper.map (BoardConfig.setNamesToDefault settings.globalSettings.defaultColumnNames)
                settings.boardConfigs
    }
