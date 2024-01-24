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
    , moveBoard
    , moveColumn
    , safeDecoder
    , switchToBoard
    , toggleIgnoreFileNameDate
    , toggleTaskCompletionInLocalTime
    , toggleTaskCompletionShowUtcOffset
    , updateDefaultColumnName
    , updateFirstDayOfWeek
    , updateTaskCompletionFormat
    )

import BoardConfig exposing (BoardConfig)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Filter exposing (Filter)
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Column exposing (ColumnForm)
import Form.Columns as ColumnsForm exposing (ColumnsForm)
import Form.NewBoard exposing (NewBoardForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.SafeDecoder as SD
import GlobalSettings exposing (GlobalSettings)
import Maybe.Extra as ME
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)
import Time



-- TYPES


type alias SettingsForm =
    { boardConfigForms : SafeZipper BoardConfigForm
    , completed : String
    , filters : List Filter
    , firstDayOfWeek : String
    , future : String
    , ignoreFileNameDates : Bool
    , otherTags : String
    , taskCompletionFormat : String
    , taskCompletionInLocalTime : Bool
    , taskCompletionShowUtcOffset : Bool
    , today : String
    , tomorrow : String
    , undated : String
    , untagged : String
    }



-- CONSTRUCTION


init : Settings -> SettingsForm
init settings =
    let
        boardConfigForms_ : SafeZipper BoardConfigForm
        boardConfigForms_ =
            SafeZipper.map BoardConfigForm.init (Settings.boardConfigs settings)

        firstDayOfWeek_ : String
        firstDayOfWeek_ =
            case Settings.globalSettings settings |> .firstDayOfWeek of
                GlobalSettings.FromLocale ->
                    "Locale"

                GlobalSettings.SpecificWeekday Time.Mon ->
                    "Mon"

                GlobalSettings.SpecificWeekday Time.Tue ->
                    "Tue"

                GlobalSettings.SpecificWeekday Time.Wed ->
                    "Wed"

                GlobalSettings.SpecificWeekday Time.Thu ->
                    "Thu"

                GlobalSettings.SpecificWeekday Time.Fri ->
                    "Fri"

                GlobalSettings.SpecificWeekday Time.Sat ->
                    "Sat"

                GlobalSettings.SpecificWeekday Time.Sun ->
                    "Sun"

        taskCompletionFormat_ : String
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

        globalSettings_ : GlobalSettings
        globalSettings_ =
            Settings.globalSettings settings

        defaultColumnNames_ : DefaultColumnNames
        defaultColumnNames_ =
            globalSettings_.defaultColumnNames
    in
    { boardConfigForms = boardConfigForms_
    , completed = Maybe.withDefault "" defaultColumnNames_.completed
    , filters = globalSettings_.filters
    , firstDayOfWeek = firstDayOfWeek_
    , future = Maybe.withDefault "" defaultColumnNames_.future
    , ignoreFileNameDates = globalSettings_.ignoreFileNameDates
    , otherTags = Maybe.withDefault "" defaultColumnNames_.otherTags
    , taskCompletionFormat = taskCompletionFormat_
    , taskCompletionInLocalTime = globalSettings_.taskCompletionInLocalTime
    , taskCompletionShowUtcOffset = globalSettings_.taskCompletionShowUtcOffset
    , today = Maybe.withDefault "" defaultColumnNames_.today
    , tomorrow = Maybe.withDefault "" defaultColumnNames_.tomorrow
    , undated = Maybe.withDefault "" defaultColumnNames_.undated
    , untagged = Maybe.withDefault "" defaultColumnNames_.untagged
    }



-- DECODER


safeDecoder : SD.Decoder SettingsForm Settings
safeDecoder =
    SD.map14 settingsBuilder
        boardConfigFormsDecoder
        completedDecoder
        filtersDecoder
        firstDayOfWeekDecoder
        futureDecoder
        ignoreFileNameDatesDecoder
        otherTagsDecoder
        taskCompletionFormatDecoder
        taskCompletionInLocalTimeDecoder
        taskCompletionShowUtcOffsetDecoder
        todayDecoder
        tomorrowDecoder
        undatedDecoder
        untaggedDecoder



-- INFO


boardConfigForms : SettingsForm -> SafeZipper BoardConfigForm
boardConfigForms form =
    form.boardConfigForms


defaultColumnNames : SettingsForm -> DefaultColumnNames
defaultColumnNames settingsForm =
    settingsForm
        |> SD.run safeDecoder
        |> Result.map Settings.defaultColumnNames
        |> Result.withDefault DefaultColumnNames.default


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
    let
        columnFormToAdd : Maybe ColumnForm
        columnFormToAdd =
            SD.run NewColumnForm.safeDecoder configToAdd
                |> Result.withDefault Nothing
    in
    mapCurrentColumnsForm (ColumnsForm.addColumn columnFormToAdd) form


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


moveBoard : String -> BeaconPosition -> SettingsForm -> SettingsForm
moveBoard draggedId beaconPosition settingsForm =
    let
        movedBoardConfigForms : SafeZipper BoardConfigForm
        movedBoardConfigForms =
            settingsForm
                |> boardConfigForms
                |> SafeZipper.toList
                |> BeaconPosition.performMove draggedId beaconPosition .name
                |> SafeZipper.fromList

        movedBoardIndex : Int
        movedBoardIndex =
            movedBoardConfigForms
                |> SafeZipper.findIndex (\c -> .name c == draggedId)
                |> Maybe.withDefault 0
    in
    { settingsForm | boardConfigForms = SafeZipper.atIndex movedBoardIndex movedBoardConfigForms }


moveColumn : String -> BeaconPosition -> SettingsForm -> SettingsForm
moveColumn draggedId beaconPosition form =
    mapCurrentColumnsForm (ColumnsForm.moveColumn draggedId beaconPosition) form


switchToBoard : Int -> SettingsForm -> SettingsForm
switchToBoard index form =
    { form | boardConfigForms = SafeZipper.atIndex index form.boardConfigForms }


toggleIgnoreFileNameDate : SettingsForm -> SettingsForm
toggleIgnoreFileNameDate settingsForm =
    { settingsForm | ignoreFileNameDates = not settingsForm.ignoreFileNameDates }


toggleTaskCompletionInLocalTime : SettingsForm -> SettingsForm
toggleTaskCompletionInLocalTime settingsForm =
    { settingsForm | taskCompletionInLocalTime = not settingsForm.taskCompletionInLocalTime }


toggleTaskCompletionShowUtcOffset : SettingsForm -> SettingsForm
toggleTaskCompletionShowUtcOffset settingsForm =
    { settingsForm | taskCompletionShowUtcOffset = not settingsForm.taskCompletionShowUtcOffset }


updateDefaultColumnName : String -> String -> SettingsForm -> SettingsForm
updateDefaultColumnName column newName settingsForm =
    case column of
        "today" ->
            { settingsForm | today = newName }

        "tomorrow" ->
            { settingsForm | tomorrow = newName }

        "future" ->
            { settingsForm | future = newName }

        "undated" ->
            { settingsForm | undated = newName }

        "otherTags" ->
            { settingsForm | otherTags = newName }

        "untagged" ->
            { settingsForm | untagged = newName }

        "completed" ->
            { settingsForm | completed = newName }

        _ ->
            settingsForm


updateFirstDayOfWeek : String -> SettingsForm -> SettingsForm
updateFirstDayOfWeek newValue settingsForm =
    { settingsForm | firstDayOfWeek = newValue }


updateTaskCompletionFormat : String -> SettingsForm -> SettingsForm
updateTaskCompletionFormat newFormat settingsForm =
    { settingsForm | taskCompletionFormat = newFormat }



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


filtersDecoder : SD.Decoder SettingsForm (List Filter)
filtersDecoder =
    SD.identity
        |> SD.lift .filters


firstDayOfWeekDecoder : SD.Decoder SettingsForm GlobalSettings.FirstDayOfWeek
firstDayOfWeekDecoder =
    (SD.custom <|
        \fdow ->
            case fdow of
                "Locale" ->
                    Ok <| GlobalSettings.FromLocale

                "Mon" ->
                    Ok <| GlobalSettings.SpecificWeekday Time.Mon

                "Tue" ->
                    Ok <| GlobalSettings.SpecificWeekday Time.Tue

                "Wed" ->
                    Ok <| GlobalSettings.SpecificWeekday Time.Wed

                "Thu" ->
                    Ok <| GlobalSettings.SpecificWeekday Time.Thu

                "Fri" ->
                    Ok <| GlobalSettings.SpecificWeekday Time.Fri

                "Sat" ->
                    Ok <| GlobalSettings.SpecificWeekday Time.Sat

                "Sun" ->
                    Ok <| GlobalSettings.SpecificWeekday Time.Sun

                _ ->
                    Ok <| GlobalSettings.FromLocale
    )
        |> SD.lift .firstDayOfWeek


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
    -> List Filter
    -> GlobalSettings.FirstDayOfWeek
    -> Maybe String
    -> Bool
    -> Maybe String
    -> GlobalSettings.TaskCompletionFormat
    -> Bool
    -> Bool
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Settings
settingsBuilder bc com fts fdw fut ifn ots tcf clt cso tod tom und unt =
    let
        defaultColumnNames_ : DefaultColumnNames
        defaultColumnNames_ =
            { today = tod
            , tomorrow = tom
            , future = fut
            , undated = und
            , otherTags = ots
            , untagged = unt
            , completed = com
            }

        globalSettings : GlobalSettings
        globalSettings =
            { defaultColumnNames = defaultColumnNames_
            , filters = fts
            , firstDayOfWeek = fdw
            , ignoreFileNameDates = ifn
            , taskCompletionFormat = tcf
            , taskCompletionInLocalTime = clt
            , taskCompletionShowUtcOffset = cso
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


taskCompletionInLocalTimeDecoder : SD.Decoder SettingsForm Bool
taskCompletionInLocalTimeDecoder =
    SD.identity
        |> SD.lift .taskCompletionInLocalTime


taskCompletionShowUtcOffsetDecoder : SD.Decoder SettingsForm Bool
taskCompletionShowUtcOffsetDecoder =
    SD.identity
        |> SD.lift .taskCompletionShowUtcOffset


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
