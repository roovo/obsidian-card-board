module Form.Settings exposing
    ( SettingsForm
    , addBoard
    , addColumn
    , boardConfigForms
    , defaultColumnNames
    , deleteColumn
    , deleteCurrentBoard
    , hasAnyBordsConfigured
    , init
    , mapCurrentBoard
    , mapCurrentColumnsForm
    , moveColumn
    , safeDecoder
    , switchToBoard
    )

import BoardConfig exposing (BoardConfig)
import Columns exposing (Columns)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm exposing (ColumnsForm)
import Form.Decoder as FD
import Form.NewBoard exposing (NewBoardForm)
import Form.NewColumn exposing (NewColumnForm)
import Form.SafeDecoder as SD
import GlobalSettings
import Maybe.Extra as ME
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)



-- TYPES


type alias SettingsForm =
    { boardConfigForms : SafeZipper BoardConfigForm
    , completed : String
    , future : String
    , ignoreFileNameDates : Bool
    , otherTags : String
    , taskCompletionFormat : String
    , today : String
    , tomorrow : String
    , undated : String
    , untagged : String
    }



-- CONSTRUCTION


init : Settings -> SettingsForm
init settings =
    let
        boardConfigForms_ =
            SafeZipper.map BoardConfigForm.init (Settings.boardConfigs settings)

        taskCompletionFormat_ =
            case Settings.globalSettings settings |> .taskCompletionFormat of
                GlobalSettings.NoCompletion ->
                    "NoCompletion"

                GlobalSettings.ObsidianCardBoard ->
                    "ObsidianCardBoard"

                GlobalSettings.ObsidianDataview ->
                    "ObsidianDataview"

                GlobalSettings.ObsidianTasks ->
                    "ObsidianTasks"

        globalSettings_ =
            Settings.globalSettings settings

        defaultColumnNames_ =
            globalSettings_.defaultColumnNames
    in
    { boardConfigForms = boardConfigForms_
    , completed = Maybe.withDefault "" defaultColumnNames_.completed
    , future = Maybe.withDefault "" defaultColumnNames_.future
    , ignoreFileNameDates = globalSettings_.ignoreFileNameDates
    , otherTags = Maybe.withDefault "" defaultColumnNames_.otherTags
    , taskCompletionFormat = taskCompletionFormat_
    , today = Maybe.withDefault "" defaultColumnNames_.today
    , tomorrow = Maybe.withDefault "" defaultColumnNames_.tomorrow
    , undated = Maybe.withDefault "" defaultColumnNames_.undated
    , untagged = Maybe.withDefault "" defaultColumnNames_.untagged
    }



-- DECODER


safeDecoder : SD.Decoder SettingsForm Settings
safeDecoder =
    SD.map10 settingsBuilder
        boardConfigFormsDecoder
        completedDecoder
        futureDecoder
        ignoreFileNameDatesDecoder
        otherTagsDecoder
        taskCompletionFormatDecoder
        todayDecoder
        tomorrowDecoder
        undatedDecoder
        untaggedDecoder



-- INFO


boardConfigForms : SettingsForm -> SafeZipper BoardConfigForm
boardConfigForms form =
    form.boardConfigForms


defaultColumnNames : SettingsForm -> DefaultColumnNames
defaultColumnNames form =
    DefaultColumnNames.default


hasAnyBordsConfigured : SettingsForm -> Bool
hasAnyBordsConfigured form =
    SafeZipper.length form.boardConfigForms /= 0



-- MODIFICATION


addBoard : DefaultColumnNames -> NewBoardForm -> SettingsForm -> SettingsForm
addBoard defaultColumnNames_ configToAdd form =
    { form
        | boardConfigForms =
            SafeZipper.last <|
                SafeZipper.add
                    (BoardConfigForm.fromNewBoardForm defaultColumnNames_ configToAdd)
                    form.boardConfigForms
    }


addColumn : NewColumnForm -> SettingsForm -> SettingsForm
addColumn configToAdd form =
    mapCurrentColumnsForm (ColumnsForm.addColumn configToAdd) form


deleteColumn : Int -> SettingsForm -> SettingsForm
deleteColumn index form =
    mapCurrentColumnsForm (ColumnsForm.deleteColumn index) form


deleteCurrentBoard : SettingsForm -> SettingsForm
deleteCurrentBoard form =
    { form | boardConfigForms = SafeZipper.deleteCurrent form.boardConfigForms }


mapCurrentBoard : (BoardConfigForm -> BoardConfigForm) -> SettingsForm -> SettingsForm
mapCurrentBoard fn form =
    { form | boardConfigForms = SafeZipper.mapSelectedAndRest fn identity form.boardConfigForms }


mapCurrentColumnsForm : (ColumnsForm -> ColumnsForm) -> SettingsForm -> SettingsForm
mapCurrentColumnsForm fn form =
    { form
        | boardConfigForms =
            SafeZipper.mapSelectedAndRest
                (BoardConfigForm.mapColumnsForm fn)
                identity
                form.boardConfigForms
    }


moveColumn : String -> BeaconPosition -> SettingsForm -> SettingsForm
moveColumn draggedId beaconPosition form =
    mapCurrentColumnsForm (ColumnsForm.moveColumn draggedId beaconPosition) form


switchToBoard : Int -> SettingsForm -> SettingsForm
switchToBoard index form =
    { form | boardConfigForms = SafeZipper.atIndex index form.boardConfigForms }



-- PRIVATE


boardConfigFormsDecoder : SD.Decoder SettingsForm (SafeZipper BoardConfig)
boardConfigFormsDecoder =
    (SD.custom <|
        \bcfs ->
            let
                currentIndex : Int
                currentIndex =
                    bcfs
                        |> SafeZipper.currentIndex
                        |> Maybe.withDefault 0
            in
            bcfs
                |> SafeZipper.toList
                |> List.map (SD.run BoardConfigForm.safeDecoder)
                |> List.map Result.toMaybe
                |> ME.values
                |> SafeZipper.fromList
                |> SafeZipper.atIndex currentIndex
                |> Ok
    )
        |> SD.lift .boardConfigForms


completedDecoder : SD.Decoder SettingsForm (Maybe String)
completedDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.map toMaybeString
        |> SD.lift .completed


futureDecoder : SD.Decoder SettingsForm (Maybe String)
futureDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.map toMaybeString
        |> SD.lift .future


ignoreFileNameDatesDecoder : SD.Decoder SettingsForm Bool
ignoreFileNameDatesDecoder =
    SD.identity
        |> SD.lift .ignoreFileNameDates


otherTagsDecoder : SD.Decoder SettingsForm (Maybe String)
otherTagsDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.map toMaybeString
        |> SD.lift .otherTags


settingsBuilder :
    SafeZipper BoardConfig
    -> Maybe String
    -> Maybe String
    -> Bool
    -> Maybe String
    -> GlobalSettings.TaskCompletionFormat
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Settings
settingsBuilder bc com fut ifn ots tcf tod tom und unt =
    let
        defaultColumnNames_ =
            { today = tod
            , tomorrow = tom
            , future = fut
            , undated = und
            , otherTags = ots
            , untagged = unt
            , completed = com
            }

        globalSettings =
            { taskCompletionFormat = tcf
            , defaultColumnNames = defaultColumnNames_
            , ignoreFileNameDates = ifn
            }
    in
    { boardConfigs = bc
    , globalSettings = globalSettings
    , version = Settings.currentVersion
    }


taskCompletionFormatDecoder : SD.Decoder SettingsForm GlobalSettings.TaskCompletionFormat
taskCompletionFormatDecoder =
    (SD.custom <|
        \tcf ->
            case tcf of
                "NoCompletion" ->
                    Ok <| GlobalSettings.NoCompletion

                "ObsidianDataview" ->
                    Ok <| GlobalSettings.ObsidianDataview

                "ObsidianTasks" ->
                    Ok <| GlobalSettings.ObsidianTasks

                _ ->
                    Ok <| GlobalSettings.ObsidianCardBoard
    )
        |> SD.lift .taskCompletionFormat


todayDecoder : SD.Decoder SettingsForm (Maybe String)
todayDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.map toMaybeString
        |> SD.lift .today


toMaybeString : String -> Maybe String
toMaybeString str =
    if String.length str == 0 then
        Nothing

    else
        Just str


tomorrowDecoder : SD.Decoder SettingsForm (Maybe String)
tomorrowDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.map toMaybeString
        |> SD.lift .tomorrow


undatedDecoder : SD.Decoder SettingsForm (Maybe String)
undatedDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.map toMaybeString
        |> SD.lift .undated


untaggedDecoder : SD.Decoder SettingsForm (Maybe String)
untaggedDecoder =
    SD.identity
        |> SD.lift String.trim
        |> SD.map toMaybeString
        |> SD.lift .untagged
