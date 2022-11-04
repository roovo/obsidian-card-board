module Page.Settings exposing
    ( Model
    , Msg(..)
    , init
    , mapSession
    , toSession
    , update
    , view
    )

import AssocList as Dict exposing (Dict)
import BoardConfig exposing (BoardConfig)
import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import FeatherIcons
import Filter exposing (Filter, Polarity)
import GlobalSettings exposing (GlobalSettings, TaskCompletionFormat)
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import InteropPorts
import List.Extra as LE
import Page.Helper.Multiselect as MultiSelect
import SafeZipper exposing (SafeZipper)
import Session exposing (Session)
import Settings exposing (Settings)
import SettingsState exposing (SettingsState)
import State exposing (State)



-- MODEL


type alias Model =
    { multiSelect : MultiSelect.Model Msg Filter
    , pathCache : State (List Filter)
    , session : Session
    , settingsState : SettingsState
    }


init : Session -> Model
init session =
    { multiSelect = MultiSelect.init multiSelectConfig (currentFilters <| Session.boardConfigs session)
    , pathCache = State.Waiting
    , session = session
    , settingsState = SettingsState.init (Session.settings session)
    }


currentFilters : SafeZipper BoardConfig -> Dict String Filter
currentFilters boardConfigs =
    boardConfigs
        |> SafeZipper.current
        |> Maybe.map BoardConfig.filters
        |> Maybe.map (\fs -> List.map (\f -> ( Filter.value f, f )) fs)
        |> Maybe.withDefault []
        |> Dict.fromList


toSession : Model -> Session
toSession =
    .session


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    { model | session = fn model.session }


multiSelectConfig : MultiSelect.Config Msg Filter
multiSelectConfig =
    { delayMs = 300
    , tagger = GotMultiSelectMsg
    , fetchMsg = PathsRequested
    , notFoundText = "Nothing Found"
    , grouper = groupedSelections
    , selectedItemLabel = selectedItemLabel
    }



-- UPDATE


type Msg
    = AddBoardClicked
    | AddBoardConfirmed
    | BackspacePressed
    | BoardNameClicked Int
    | BoardTypeSelected String
    | DeleteBoardRequested
    | DeleteBoardConfirmed
    | EnteredCompletedCount String
    | EnteredNewBoardTitle String
    | EnteredTags String
    | EnteredTitle String
    | FilterCandidatesReceived (List Filter)
    | GlobalSettingsClicked
    | GotMultiSelectMsg (MultiSelect.Msg Msg Filter)
    | ModalCancelClicked
    | ModalCloseClicked
    | PathsRequested Int String
    | PolaritySelected String
    | TaskCompletionFormatSelected String
    | ToggleIncludeOthers
    | ToggleIncludeUndated
    | ToggleIncludeUntagged
    | ToggleShowColumnTags
    | ToggleShowFilteredTags


switchSettingsState : (SettingsState -> SettingsState) -> Model -> Model
switchSettingsState fn model =
    let
        currentSelectedFilters : List Filter
        currentSelectedFilters =
            Dict.values <| MultiSelect.selectedItems model.multiSelect

        newSettingsState : SettingsState
        newSettingsState =
            model.settingsState
                |> SettingsState.mapBoardBeingEdited (BoardConfig.updateFilters currentSelectedFilters)
                |> fn

        newFilters : Dict String Filter
        newFilters =
            currentFilters <| SettingsState.boardConfigs newSettingsState
    in
    { model
        | settingsState = newSettingsState
        , multiSelect = MultiSelect.updateSelectedItems newFilters model.multiSelect
    }


update : Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update msg model =
    case msg of
        AddBoardClicked ->
            mapSettingsState SettingsState.addBoardRequested model

        AddBoardConfirmed ->
            wrap <| switchSettingsState SettingsState.confirmAddBoard model

        BackspacePressed ->
            wrap { model | multiSelect = MultiSelect.deleteHighlightedItem model.multiSelect }

        BoardNameClicked index ->
            wrap <| switchSettingsState (SettingsState.editBoardAt index) model

        BoardTypeSelected boardType ->
            mapBoardBeingAdded (BoardConfig.updateBoardType boardType) model

        DeleteBoardRequested ->
            mapSettingsState SettingsState.deleteBoardRequested model

        DeleteBoardConfirmed ->
            wrap <| switchSettingsState SettingsState.confirmDeleteBoard model

        EnteredCompletedCount value ->
            mapBoardBeingEdited (BoardConfig.updateCompletedCount (String.toInt value)) model

        EnteredNewBoardTitle title ->
            mapBoardBeingAdded (BoardConfig.updateTitle title) model

        EnteredTags tags ->
            mapBoardBeingEdited (BoardConfig.updateTags tags) model

        EnteredTitle title ->
            mapBoardBeingEdited (BoardConfig.updateTitle title) model

        FilterCandidatesReceived filterCandidates ->
            let
                multiSelectModel : MultiSelect.Model Msg Filter
                multiSelectModel =
                    filterCandidates
                        |> List.map (\p -> { label = Filter.value p, value = p })
                        |> MultiSelect.recieveItems model.multiSelect
            in
            wrap
                { model
                    | pathCache = State.Loaded filterCandidates
                    , multiSelect = multiSelectModel
                }

        GotMultiSelectMsg mulSelMsg ->
            let
                ( newModel, cmd ) =
                    MultiSelect.update mulSelMsg model.multiSelect
            in
            ( { model | multiSelect = newModel }
            , cmd
            , Session.NoOp
            )

        GlobalSettingsClicked ->
            mapSettingsState SettingsState.editGlobalSettings model

        ModalCancelClicked ->
            handleClose model

        ModalCloseClicked ->
            handleClose model

        PathsRequested page searchTerm ->
            let
                cmd : Cmd Msg
                cmd =
                    case model.pathCache of
                        State.Waiting ->
                            InteropPorts.requestFilterCandidates

                        State.Loading _ ->
                            Cmd.none

                        State.Loaded _ ->
                            Cmd.none
            in
            ( model
            , cmd
            , Session.NoOp
            )

        PolaritySelected polarity ->
            mapBoardBeingEdited (BoardConfig.updateFilterPolarity polarity) model

        TaskCompletionFormatSelected taskCompletionFormat ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            (GlobalSettings.updateTaskCompletionFormat taskCompletionFormat)
                            model.settingsState
                }

        ToggleIncludeOthers ->
            mapBoardBeingEdited BoardConfig.toggleIncludeOthers model

        ToggleIncludeUndated ->
            mapBoardBeingEdited BoardConfig.toggleIncludeUndated model

        ToggleIncludeUntagged ->
            mapBoardBeingEdited BoardConfig.toggleIncludeUntagged model

        ToggleShowColumnTags ->
            mapBoardBeingEdited BoardConfig.toggleShowColumnTags model

        ToggleShowFilteredTags ->
            mapBoardBeingEdited BoardConfig.toggleShowFilteredTags model


selectedItemLabel : Filter -> String
selectedItemLabel filter =
    filter
        |> Filter.filterType
        |> String.toLower
        |> String.dropRight 1


groupedSelections : List (MultiSelect.SelectionItem Filter) -> List ( String, List (MultiSelect.SelectionItem Filter) )
groupedSelections selectionItems =
    selectionItems
        |> List.sortBy (\item -> Filter.filterType item.value)
        |> LE.groupWhile (\a b -> Filter.filterType a.value == Filter.filterType b.value)
        |> List.map (\( i, is ) -> ( Filter.filterType i.value, i :: is ))
        |> ensureAllTypes


ensureAllTypes : List ( String, List (MultiSelect.SelectionItem Filter) ) -> List ( String, List (MultiSelect.SelectionItem Filter) )
ensureAllTypes list =
    let
        hasType : String -> ( String, List (MultiSelect.SelectionItem Filter) ) -> Bool
        hasType filterType ( itemFilterType, _ ) =
            filterType == itemFilterType

        typeOrDefault : String -> ( String, List (MultiSelect.SelectionItem Filter) )
        typeOrDefault filterType =
            LE.find (hasType filterType) list
                |> Maybe.withDefault ( filterType, [] )
    in
    List.map typeOrDefault Filter.filterTypes


handleClose : Model -> ( Model, Cmd Msg, Session.Msg )
handleClose model =
    let
        newModel : Model
        newModel =
            switchSettingsState SettingsState.cancelCurrentState model

        newSettings : Settings
        newSettings =
            SettingsState.settings newModel.settingsState
    in
    case newModel.settingsState of
        SettingsState.ClosingPlugin _ ->
            ( newModel
            , Cmd.batch
                [ InteropPorts.updateSettings newSettings
                , InteropPorts.closeView
                ]
            , Session.SettingsClosed newSettings
            )

        SettingsState.ClosingSettings _ ->
            ( newModel
            , InteropPorts.updateSettings newSettings
            , Session.SettingsClosed newSettings
            )

        _ ->
            wrap newModel


mapSettingsState : (SettingsState -> SettingsState) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapSettingsState fn model =
    wrap { model | settingsState = fn model.settingsState }


mapBoardBeingAdded : (BoardConfig -> BoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapBoardBeingAdded fn model =
    wrap { model | settingsState = SettingsState.mapBoardBeingAdded fn model.settingsState }


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapBoardBeingEdited fn model =
    wrap { model | settingsState = SettingsState.mapBoardBeingEdited fn model.settingsState }


wrap : Model -> ( Model, Cmd Msg, Session.Msg )
wrap model =
    ( model, Cmd.none, Session.NoOp )



-- VIEW


type CurrentSection
    = Options
    | Boards


view : Model -> Html Msg
view model =
    case model.settingsState of
        SettingsState.AddingBoard newConfig settings ->
            Html.div []
                [ boardSettingsView (Settings.boardConfigs settings) model.multiSelect
                , modalAddBoard newConfig
                ]

        SettingsState.ClosingPlugin _ ->
            Html.text ""

        SettingsState.ClosingSettings _ ->
            Html.text ""

        SettingsState.DeletingBoard settings ->
            Html.div []
                [ boardSettingsView (Settings.boardConfigs settings) model.multiSelect
                , modalConfirmDelete
                ]

        SettingsState.EditingBoard settings ->
            boardSettingsView (Settings.boardConfigs settings) model.multiSelect

        SettingsState.EditingGlobalSettings settings ->
            globalSettingsView (Session.dataviewTaskCompletion <| toSession model) settings


modalAddBoard : BoardConfig -> Html Msg
modalAddBoard newConfig =
    Html.div [ class "modal-container" ]
        [ Html.div [ class "modal-bg" ] []
        , Html.div [ class "modal" ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-title" ]
                [ Html.text "Add new board" ]
            , Html.div [ class "modal-form" ]
                [ Html.div [ class "form-item" ]
                    [ Html.div [ class "form-item-name" ]
                        [ Html.text "Title" ]
                    , Html.div [ class "form-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value <| BoardConfig.title newConfig
                            , onInput EnteredNewBoardTitle
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "form-item" ]
                    [ Html.div [ class "form-item-name" ]
                        [ Html.text "Type" ]
                    , Html.div [ class "form-item-control" ]
                        [ Html.select
                            [ class "dropdown"
                            , onInput BoardTypeSelected
                            ]
                            [ Html.option
                                [ value "dateBoard"
                                , selected <| BoardConfig.isForDateBoard newConfig
                                ]
                                [ Html.text "Date board" ]
                            , Html.option
                                [ value "tagBoard"
                                , selected <| BoardConfig.isForTagBoard newConfig
                                ]
                                [ Html.text "Tag board" ]
                            ]
                        ]
                    ]
                , Html.div [ class "dialog-buttons" ]
                    [ Html.button
                        [ onClick <| ModalCancelClicked
                        ]
                        [ Html.text "Cancel"
                        ]
                    , Html.button
                        [ class "mod-cta"
                        , onClick <| AddBoardConfirmed
                        ]
                        [ Html.text "Add"
                        ]
                    ]
                ]
            ]
        ]


modalConfirmDelete : Html Msg
modalConfirmDelete =
    Html.div [ class "modal-container" ]
        [ Html.div [ class "modal-bg" ] []
        , Html.div [ class "modal" ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-title" ]
                [ Html.text "Confirm Deletion" ]
            , Html.div [ class "dialog-buttons" ]
                [ Html.button
                    [ onClick <| ModalCancelClicked
                    ]
                    [ Html.text "Do not delete"
                    ]
                , Html.button
                    [ class "mod-warning"
                    , onClick <| DeleteBoardConfirmed
                    ]
                    [ Html.text "Go ahead"
                    ]
                ]
            ]
        ]


settingsSurroundView : CurrentSection -> SafeZipper BoardConfig -> List (Html Msg) -> Html Msg
settingsSurroundView currentSection configs formContents =
    let
        boardMapFn : Int -> BoardConfig -> Html Msg
        boardMapFn =
            case currentSection of
                Options ->
                    settingTitleView

                Boards ->
                    settingTitleSelectedView

        globalSettingsClass : String
        globalSettingsClass =
            case currentSection of
                Options ->
                    "vertical-tab-nav-item is-active"

                Boards ->
                    "vertical-tab-nav-item"
    in
    Html.div [ class "modal-container" ]
        [ Html.div
            [ class "modal-bg"
            , onClick ModalCloseClicked
            ]
            []
        , Html.div [ class "modal mod-settings mod-sidebar-layout" ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-title" ]
                [ Html.text "The Modal Title" ]
            , Html.div [ class "modal-content vertical-tabs-container" ]
                [ Html.div [ class "settings-menu vertical-tab-header" ]
                    [ Html.div [ class "vertical-tab-header-group" ]
                        [ Html.div [ class "vertical-tab-header-group-title" ]
                            [ Html.text "Options" ]
                        , Html.div [ class "vertical-tab-header-group-items" ]
                            [ Html.div
                                [ class globalSettingsClass
                                , onClick GlobalSettingsClicked
                                ]
                                [ Html.text "Global Settings" ]
                            ]
                        ]
                    , Html.div [ class "vertical-tab-header-group" ]
                        [ Html.div [ class "vertical-tab-header-group-title" ]
                            [ Html.text "Boards"
                            , Html.div
                                [ class "vertical-tab-header-group-title-icon"
                                , onClick AddBoardClicked
                                ]
                                [ FeatherIcons.plus
                                    |> FeatherIcons.withSize 1
                                    |> FeatherIcons.withSizeUnit "em"
                                    |> FeatherIcons.toHtml []
                                ]
                            ]
                        , Html.div [ class "vertical-tab-header-group-items" ]
                            (configs
                                |> SafeZipper.indexedMapSelectedAndRest boardMapFn settingTitleView
                                |> SafeZipper.toList
                            )
                        ]
                    ]
                , Html.div [ class "vertical-tab-content-container" ]
                    [ Html.div [ class "vertical-tab-content" ]
                        formContents
                    ]
                ]
            ]
        ]


boardSettingsView : SafeZipper BoardConfig -> MultiSelect.Model Msg Filter -> Html Msg
boardSettingsView boardConfigs multiselect =
    boardSettingsForm (SafeZipper.current boardConfigs) (SafeZipper.currentIndex boardConfigs) multiselect
        |> settingsSurroundView Boards boardConfigs


globalSettingsView : DataviewTaskCompletion -> Settings -> Html Msg
globalSettingsView dataviewTaskCompletion settings =
    settings
        |> Settings.globalSettings
        |> globalSettingsForm dataviewTaskCompletion
        |> settingsSurroundView Options (Settings.boardConfigs settings)


globalSettingsForm : DataviewTaskCompletion -> GlobalSettings -> List (Html Msg)
globalSettingsForm dataviewTaskCompletion gs =
    let
        dataViewExample : String
        dataViewExample =
            case dataviewTaskCompletion of
                DataviewTaskCompletion.NoCompletion ->
                    "automatic task completion tracking disabled"

                DataviewTaskCompletion.Emoji ->
                    "✅ 1999-12-31"

                DataviewTaskCompletion.Text t ->
                    "[" ++ t ++ ":: 1999-12-31]"
    in
    [ Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Task completion format" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Which format to use when marking tasks as completed:"
                , Html.br [] []
                , Html.br [] []
                , Html.strong [] [ Html.text "None" ]
                , Html.text ": "
                , Html.code [] [ Html.text "no completion date/time will be added" ]
                , Html.br [] []
                , Html.strong [] [ Html.text "CardBoard" ]
                , Html.text ": "
                , Html.code [] [ Html.text "@completed(1999-12-31T23:59:59)" ]
                , Html.br [] []
                , Html.strong [] [ Html.text "Dataview" ]
                , Html.text ": "
                , Html.code [] [ Html.text dataViewExample ]
                , Html.i [] [ Html.text " (change via Dataview plugin settings)" ]
                , Html.br [] []
                , Html.strong [] [ Html.text "Tasks" ]
                , Html.text ": "
                , Html.code [] [ Html.text "✅ 1999-12-31" ]
                , Html.br [] []
                , Html.br [] []
                , Html.strong [] [ Html.text "For Dataview: " ]
                , Html.text "The task related settings in the Dataview plugin (if installed) will be respected"
                , Html.text " and should be reflected above.  The only exception is that the completion date format"
                , Html.text " is ignored and yyyy-MM-dd is what CardBoard uses."
                , Html.i [] [ Html.text " If you change the settings in" ]
                , Html.i [] [ Html.text " Dataview you will need to close and re-open the CardBoard view to pick up the changes." ]
                , Html.br [] []
                , Html.br [] []
                , Html.text "When reading tasks, CardBoard understands all these formats.  It also understands"
                , Html.text " due dates whether in CardBoard, Dataview, or Tasks format."
                ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ taskCompletionFormatSelect gs.taskCompletionFormat ]
        ]
    ]


boardSettingsForm : Maybe BoardConfig -> Maybe Int -> MultiSelect.Model Msg Filter -> List (Html Msg)
boardSettingsForm boardConfig boardIndex multiselect =
    case ( boardConfig, boardIndex ) of
        ( Just (BoardConfig.DateBoardConfig config), Just index ) ->
            let
                showFilteredTagsStyle : String
                showFilteredTagsStyle =
                    if config.showFilteredTags then
                        " is-enabled"

                    else
                        ""

                includeUndatedStyle : String
                includeUndatedStyle =
                    if config.includeUndated then
                        " is-enabled"

                    else
                        ""
            in
            [ Html.div [ class "setting-items-inner" ]
                [ Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Title" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "The name of this board" ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value config.title
                            , onInput EnteredTitle
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item setting-item-heading" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Filters" ]
                        , Html.div [ class "setting-item-description" ] []
                        ]
                    , Html.div [ class "setting-item-control" ] []
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Definitions" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Filter tasks by files, paths, and/or tags." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ MultiSelect.view multiselect
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Polarity" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Use the filters as an Allow or Deny list." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ polaritySelect config.filterPolarity ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Show filter tags on cards" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Turn this on to show the tags used in filters on cards on this board." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.div
                            [ class <| "checkbox-container" ++ showFilteredTagsStyle
                            , onClick ToggleShowFilteredTags
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item setting-item-heading" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Columns" ]
                        , Html.div [ class "setting-item-description" ] []
                        ]
                    , Html.div [ class "setting-item-control" ] []
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Include undated" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Whether to include a column for tasks with no due date." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.div
                            [ class <| "checkbox-container" ++ includeUndatedStyle
                            , onClick ToggleIncludeUndated
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Completed count" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "How many completed tasks to show.  Set to zero to disable the completed column altogether." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value <| String.fromInt config.completedCount
                            , onInput EnteredCompletedCount
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item dialog-buttons" ]
                    [ Html.button
                        [ class "mod-warning"
                        , onClick <| DeleteBoardRequested
                        ]
                        [ Html.text "Delete this board"
                        ]
                    ]
                ]
            ]

        ( Just (BoardConfig.TagBoardConfig config), Just index ) ->
            let
                showFilteredTagsStyle : String
                showFilteredTagsStyle =
                    if config.showFilteredTags then
                        " is-enabled"

                    else
                        ""

                showColumnTagsStyle : String
                showColumnTagsStyle =
                    if config.showColumnTags then
                        " is-enabled"

                    else
                        ""

                includeOthersStyle : String
                includeOthersStyle =
                    if config.includeOthers then
                        " is-enabled"

                    else
                        ""

                includeUntaggedStyle : String
                includeUntaggedStyle =
                    if config.includeUntagged then
                        " is-enabled"

                    else
                        ""

                tagText : String
                tagText =
                    config.columns
                        |> List.map (\c -> "#" ++ c.tag ++ " " ++ c.displayTitle)
                        |> String.join "\n"
            in
            [ Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Title" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "The name of this board" ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.input
                        [ type_ "text"
                        , value config.title
                        , onInput EnteredTitle
                        ]
                        []
                    ]
                ]
            , Html.div [ class "setting-item setting-item-heading" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Filters" ]
                    , Html.div [ class "setting-item-description" ] []
                    ]
                , Html.div [ class "setting-item-control" ] []
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Definitions" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Filter tasks by files, paths, and/or tags." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ MultiSelect.view multiselect
                    ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Polarity" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Use the filters defined above as an Allow or Deny list." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ polaritySelect config.filterPolarity ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Show filter tags on cards" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Turn this on to show the tags used in filters on cards on this board." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.div
                        [ class <| "checkbox-container" ++ showFilteredTagsStyle
                        , onClick ToggleShowFilteredTags
                        ]
                        []
                    ]
                ]
            , Html.div [ class "setting-item setting-item-heading" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Columns" ]
                    , Html.div [ class "setting-item-description" ] []
                    ]
                , Html.div [ class "setting-item-control" ] []
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Definitions" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.div []
                            [ Html.text "The tags to use to define board columns." ]
                        , Html.div []
                            [ Html.text
                                ("Each line should be a tag followed by the column heading.  "
                                    ++ "Add a trailing / to the tag to include tasks with any subtags in the column too.  "
                                    ++ "If you do not specify the heading it will be auto-generated from the tag."
                                )
                            ]
                        ]
                    ]
                , Html.Keyed.node "div"
                    [ class "setting-item-control" ]
                    [ ( String.fromInt index
                      , Html.textarea
                            [ onInput EnteredTags
                            , placeholder "#tag1 Column heading\n#tag2/\n#tag3/subtag\ntag4"
                            ]
                            [ Html.text tagText ]
                      )
                    ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Show column tags on cards" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Turn this on if you want to display the tags used to define columns on cards on this board." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.div
                        [ class <| "checkbox-container" ++ showColumnTagsStyle
                        , onClick ToggleShowColumnTags
                        ]
                        []
                    ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Include others" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Whether to include a column for tasks with tags other than those specified." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.div
                        [ class <| "checkbox-container" ++ includeOthersStyle
                        , onClick ToggleIncludeOthers
                        ]
                        []
                    ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Include untagged" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Whether to include a column for tasks with no tags." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.div
                        [ class <| "checkbox-container" ++ includeUntaggedStyle
                        , onClick ToggleIncludeUntagged
                        ]
                        []
                    ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Completed count" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "How many completed tasks to show.  Set to zero to disable the completed column altogether." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.input
                        [ type_ "text"
                        , value <| String.fromInt config.completedCount
                        , onInput EnteredCompletedCount
                        ]
                        []
                    ]
                ]
            , Html.div [ class "setting-item dialog-buttons" ]
                [ Html.button
                    [ class "mod-warning"
                    , onClick <| DeleteBoardRequested
                    ]
                    [ Html.text "Delete this board"
                    ]
                ]
            ]

        _ ->
            [ Html.text "" ]


taskCompletionFormatSelect : TaskCompletionFormat -> Html Msg
taskCompletionFormatSelect taskCompletionFormat =
    Html.select
        [ class "dropdown"
        , onInput TaskCompletionFormatSelected
        ]
        (case taskCompletionFormat of
            GlobalSettings.NoCompletion ->
                [ Html.option [ value "NoCompletion", selected True ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard" ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview" ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks" ]
                    [ Html.text "Tasks" ]
                ]

            GlobalSettings.ObsidianCardBoard ->
                [ Html.option [ value "NoCompletion" ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard", selected True ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview" ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks" ]
                    [ Html.text "Tasks" ]
                ]

            GlobalSettings.ObsidianDataview ->
                [ Html.option [ value "NoCompletion" ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard" ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview", selected True ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks" ]
                    [ Html.text "Tasks" ]
                ]

            GlobalSettings.ObsidianTasks ->
                [ Html.option [ value "NoCompletion" ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard" ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview" ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks", selected True ]
                    [ Html.text "Tasks" ]
                ]
        )


polaritySelect : Polarity -> Html Msg
polaritySelect polarity =
    Html.select
        [ class "dropdown"
        , onInput PolaritySelected
        ]
        (case polarity of
            Filter.Allow ->
                [ Html.option [ value "Allow", selected True ]
                    [ Html.text "Allow" ]
                , Html.option [ value "Deny" ]
                    [ Html.text "Deny" ]
                ]

            Filter.Deny ->
                [ Html.option [ value "Allow" ]
                    [ Html.text "Allow" ]
                , Html.option [ value "Deny", selected True ]
                    [ Html.text "Deny" ]
                ]
        )


settingTitleSelectedView : Int -> BoardConfig -> Html Msg
settingTitleSelectedView index boardConfig =
    Html.div
        [ class "vertical-tab-nav-item is-active"
        , onClick <| BoardNameClicked index
        ]
        [ Html.text <| BoardConfig.title boardConfig ]


settingTitleView : Int -> BoardConfig -> Html Msg
settingTitleView index boardConfig =
    Html.div
        [ class "vertical-tab-nav-item"
        , onClick <| BoardNameClicked index
        ]
        [ Html.text <| BoardConfig.title boardConfig ]
