module Settings exposing
    ( Settings
    , addBoard
    , boardConfigs
    , cleanupTitles
    , currentVersion
    , decoder
    , default
    , deleteCurrentBoard
    , encoder
    , globalSettings
    , hasAnyBordsConfigured
    , mapGlobalSettings
    , moveBoard
    , switchToBoard
    , updateBoardConfigs
    , updateCurrentBoard
    , updatePath
    )

import BoardConfig exposing (BoardConfig)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Filter
import GlobalSettings exposing (GlobalSettings)
import List.Extra as LE
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


cleanupTitles : Settings -> Settings
cleanupTitles settings =
    case SafeZipper.currentIndex settings.boardConfigs of
        Just index ->
            settings
                |> replaceMissingBoardTitles
                |> enforceUniqueBoardTitles
                |> boardConfigs
                |> SafeZipper.atIndex index
                |> (\cs -> { settings | boardConfigs = cs })

        Nothing ->
            settings


currentVersion : Semver.Version
currentVersion =
    Semver.version 0 10 0 [] []


globalSettings : Settings -> GlobalSettings
globalSettings =
    .globalSettings


hasAnyBordsConfigured : Settings -> Bool
hasAnyBordsConfigured settings =
    SafeZipper.length settings.boardConfigs /= 0



-- TRANSFORM


addBoard : BoardConfig -> Settings -> Settings
addBoard configToAdd settings =
    { settings | boardConfigs = SafeZipper.last <| SafeZipper.add configToAdd settings.boardConfigs }


deleteCurrentBoard : Settings -> Settings
deleteCurrentBoard settings =
    { settings | boardConfigs = SafeZipper.deleteCurrent settings.boardConfigs }


mapGlobalSettings : (GlobalSettings -> GlobalSettings) -> Settings -> Settings
mapGlobalSettings fn settings =
    { settings | globalSettings = fn settings.globalSettings }


moveBoard : String -> BeaconPosition -> Settings -> Settings
moveBoard draggedId beaconPosition settings =
    if BeaconPosition.identifier beaconPosition == draggedId then
        settings

    else
        let
            boardList : List BoardConfig
            boardList =
                boardConfigs settings
                    |> SafeZipper.toList

            found : Maybe BoardConfig
            found =
                List.filter (\c -> BoardConfig.title c == draggedId) boardList
                    |> List.head

            afterRemoving : List BoardConfig
            afterRemoving =
                List.filter (\c -> BoardConfig.title c /= draggedId) boardList

            insertConfig : BoardConfig -> Result String (List BoardConfig)
            insertConfig foundConfig =
                case LE.findIndex (\c -> BoardConfig.title c == BeaconPosition.identifier beaconPosition) afterRemoving of
                    Nothing ->
                        Err ""

                    Just beaconIndex ->
                        case beaconPosition of
                            BeaconPosition.After _ ->
                                Ok
                                    (List.concat
                                        [ List.take (beaconIndex + 1) afterRemoving
                                        , [ foundConfig ]
                                        , List.drop (beaconIndex + 1) afterRemoving
                                        ]
                                    )

                            BeaconPosition.Before _ ->
                                Ok
                                    (List.concat
                                        [ List.take beaconIndex afterRemoving
                                        , [ foundConfig ]
                                        , List.drop beaconIndex afterRemoving
                                        ]
                                    )
        in
        case found of
            Nothing ->
                settings

            Just foundConfig ->
                case insertConfig foundConfig of
                    Err _ ->
                        settings

                    Ok newList ->
                        { settings | boardConfigs = SafeZipper.fromList newList }


switchToBoard : Int -> Settings -> Settings
switchToBoard index settings =
    { settings | boardConfigs = SafeZipper.atIndex index settings.boardConfigs }


updateBoardConfigs : SafeZipper BoardConfig -> Settings -> Settings
updateBoardConfigs newConfigs settings =
    { settings | boardConfigs = newConfigs }


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
        |> TsDecode.map cleanupTitles



-- PRIVATE


dataEncoder : TsEncode.Encoder { a | boardConfigs : SafeZipper BoardConfig, globalSettings : GlobalSettings }
dataEncoder =
    TsEncode.object
        [ TsEncode.required "boardConfigs" .boardConfigs (TsEncode.map SafeZipper.toList <| TsEncode.list BoardConfig.encoder)
        , TsEncode.required "globalSettings" .globalSettings GlobalSettings.encoder
        ]


enforceUniqueBoardTitles : Settings -> Settings
enforceUniqueBoardTitles settings =
    let
        helper : Int -> BoardConfig -> ( List String, SafeZipper BoardConfig ) -> ( List String, SafeZipper BoardConfig )
        helper index config ( titles, cs ) =
            let
                uniqueTitle : String
                uniqueTitle =
                    if List.member (String.replace " " "_" <| BoardConfig.title config) titles then
                        BoardConfig.title config ++ "." ++ String.fromInt index

                    else
                        BoardConfig.title config

                safeConfig =
                    BoardConfig.updateTitle uniqueTitle config

                ts =
                    String.replace " " "_" uniqueTitle :: titles
            in
            ( ts, SafeZipper.add safeConfig cs )

        withUniqueBoardTitles : SafeZipper BoardConfig
        withUniqueBoardTitles =
            SafeZipper.indexedFoldl helper ( [], SafeZipper.empty ) settings.boardConfigs
                |> Tuple.second
    in
    { settings | boardConfigs = withUniqueBoardTitles }


replaceMissingBoardTitles : Settings -> Settings
replaceMissingBoardTitles settings =
    let
        withNoMissingTitles : BoardConfig -> BoardConfig
        withNoMissingTitles config =
            if String.isEmpty (String.trim <| BoardConfig.title config) then
                BoardConfig.updateTitle "Untitled" config

            else
                config
    in
    { settings | boardConfigs = SafeZipper.map withNoMissingTitles settings.boardConfigs }


semverEncoder : TsEncode.Encoder Semver.Version
semverEncoder =
    TsEncode.string
        |> TsEncode.map Semver.print


versionedSettingsDecoder : TsDecode.AndThenContinuation (String -> TsDecode.Decoder Settings)
versionedSettingsDecoder =
    TsDecode.andThenInit
        (\v_0_10_0 v_0_9_0 v_0_8_0 v_0_7_0 v_0_6_0 v_0_5_0 v_0_4_0 v_0_3_0 v_0_2_0 v_0_1_0 unsupportedVersion version_ ->
            case version_ of
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


v_0_10_0_Decoder : TsDecode.Decoder Settings
v_0_10_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_10_0)))
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_10_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_9_0_Decoder : TsDecode.Decoder Settings
v_0_9_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_9_0)))
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_9_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_8_0_Decoder : TsDecode.Decoder Settings
v_0_8_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_6_0)))
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_7_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_7_0_Decoder : TsDecode.Decoder Settings
v_0_7_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_5_0)))
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_7_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_6_0_Decoder : TsDecode.Decoder Settings
v_0_6_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_5_0)))
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_6_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_5_0_Decoder : TsDecode.Decoder Settings
v_0_5_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_5_0)))
        |> TsDecode.andMap (TsDecode.field "globalSettings" GlobalSettings.v_0_5_0_decoder)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_4_0_Decoder : TsDecode.Decoder Settings
v_0_4_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_4_0)))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_3_0_Decoder : TsDecode.Decoder Settings
v_0_3_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_3_0)))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_2_0_Decoder : TsDecode.Decoder Settings
v_0_2_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_2_0)))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


v_0_1_0_Decoder : TsDecode.Decoder Settings
v_0_1_0_Decoder =
    TsDecode.succeed Settings
        |> TsDecode.andMap (TsDecode.field "boardConfigs" (TsDecode.map SafeZipper.fromList (TsDecode.list BoardConfig.decoder_v_0_1_0)))
        |> TsDecode.andMap (TsDecode.succeed GlobalSettings.default)
        |> TsDecode.andMap (TsDecode.succeed currentVersion)


unsupportedVersionDecoder : TsDecode.Decoder Settings
unsupportedVersionDecoder =
    TsDecode.fail "Unsupported settings file version"
