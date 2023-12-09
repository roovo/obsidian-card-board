module Settings exposing
    ( Settings
    , addBoard
    , boardConfigs
    , cleanupNames
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
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Filter
import GlobalSettings exposing (GlobalSettings)
import List.Extra as LE
import NewBoardConfig exposing (NewBoardConfig)
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
                |> boardConfigs
                |> SafeZipper.atIndex index
                |> (\cs -> { settings | boardConfigs = cs })

        Nothing ->
            settings


currentVersion : Semver.Version
currentVersion =
    Semver.version 0 11 0 [] []


globalSettings : Settings -> GlobalSettings
globalSettings =
    .globalSettings


hasAnyBordsConfigured : Settings -> Bool
hasAnyBordsConfigured settings =
    SafeZipper.length settings.boardConfigs /= 0



-- TRANSFORM


addBoard : DefaultColumnNames -> NewBoardConfig -> Settings -> Settings
addBoard defaultColumnNames configToAdd settings =
    { settings
        | boardConfigs =
            SafeZipper.last <|
                SafeZipper.add
                    (BoardConfig.fromNewBoardConfig defaultColumnNames configToAdd)
                    settings.boardConfigs
    }


deleteCurrentBoard : Settings -> Settings
deleteCurrentBoard settings =
    { settings | boardConfigs = SafeZipper.deleteCurrent settings.boardConfigs }


mapGlobalSettings : (GlobalSettings -> GlobalSettings) -> Settings -> Settings
mapGlobalSettings fn settings =
    { settings | globalSettings = fn settings.globalSettings }


moveBoard : String -> BeaconPosition -> Settings -> Settings
moveBoard draggedId beaconPosition settings =
    if BeaconPosition.uniqueId beaconPosition == draggedId then
        let
            boardList : List BoardConfig
            boardList =
                boardConfigs settings
                    |> SafeZipper.toList
        in
        case LE.findIndex (\c -> BoardConfig.name c == BeaconPosition.uniqueId beaconPosition) boardList of
            Nothing ->
                settings

            Just boardIndex ->
                { settings
                    | boardConfigs =
                        SafeZipper.fromList boardList
                            |> SafeZipper.atIndex boardIndex
                }

    else
        let
            boardList : List BoardConfig
            boardList =
                boardConfigs settings
                    |> SafeZipper.toList

            found : Maybe BoardConfig
            found =
                List.filter (\c -> BoardConfig.name c == draggedId) boardList
                    |> List.head

            afterRemoving : List BoardConfig
            afterRemoving =
                List.filter (\c -> BoardConfig.name c /= draggedId) boardList

            insertConfig : BoardConfig -> Result String ( List BoardConfig, Int )
            insertConfig foundConfig =
                case LE.findIndex (\c -> BoardConfig.name c == BeaconPosition.uniqueId beaconPosition) afterRemoving of
                    Nothing ->
                        Err ""

                    Just beaconIndex ->
                        case beaconPosition of
                            BeaconPosition.After _ ->
                                Ok
                                    ( List.concat
                                        [ List.take (beaconIndex + 1) afterRemoving
                                        , [ foundConfig ]
                                        , List.drop (beaconIndex + 1) afterRemoving
                                        ]
                                    , beaconIndex + 1
                                    )

                            BeaconPosition.Before _ ->
                                Ok
                                    ( List.concat
                                        [ List.take beaconIndex afterRemoving
                                        , [ foundConfig ]
                                        , List.drop beaconIndex afterRemoving
                                        ]
                                    , beaconIndex
                                    )
        in
        case found of
            Nothing ->
                settings

            Just foundConfig ->
                case insertConfig foundConfig of
                    Err _ ->
                        settings

                    Ok ( newList, movedBoardIndex ) ->
                        { settings
                            | boardConfigs =
                                SafeZipper.fromList newList
                                    |> SafeZipper.atIndex movedBoardIndex
                        }


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
