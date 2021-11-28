module Page.Settings exposing
    ( Model
    , Msg(..)
    , init
    , mapSession
    , mapSessionConfig
    , toSession
    , update
    , view
    )

import AssocList as Dict exposing (Dict)
import BoardConfig exposing (BoardConfig)
import FeatherIcons
import Flip
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import InteropPorts
import Page.Helper.Multiselect as MultiSelect
import Parser
import SafeZipper exposing (SafeZipper)
import Session exposing (Session)
import SettingsState exposing (SettingsState)
import State exposing (State)
import TagBoard



-- MODEL


type alias Model =
    { boardConfigs : SafeZipper BoardConfig
    , multiSelect : MultiSelect.Model Msg String
    , pathCache : State (List String)
    , session : Session
    , settingsState : SettingsState
    }


init : Session -> Model
init session =
    { boardConfigs = Session.boardConfigs session
    , multiSelect = MultiSelect.init multiSelectConfig Dict.empty
    , pathCache = State.Waiting
    , session = session
    , settingsState = SettingsState.init (Session.boardConfigs session)
    }


toSession : Model -> Session
toSession =
    .session


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    { model | session = fn model.session }


mapSessionConfig : (Session.Config -> Session.Config) -> Model -> Model
mapSessionConfig fn model =
    { model | session = Session.mapConfig fn model.session }


multiSelectConfig : MultiSelect.Config Msg String
multiSelectConfig =
    { delayMs = 300
    , tagger = GotMultiSelectMsg
    , fetchMsg = PathsRequested
    , notFoundText = "Nothing Found"
    }



-- UPDATE


type Msg
    = AddBoardClicked
    | AddBoardConfirmed
    | BoardTypeSelected String
    | DeleteBoardRequested
    | DeleteBoardConfirmed
    | EnteredCompletedCount String
    | EnteredNewBoardTitle String
    | EnteredTags String
    | EnteredTitle String
    | FolderPathsReceived (List String)
    | GotMultiSelectMsg (MultiSelect.Msg Msg String)
    | ModalCancelClicked
    | ModalCloseClicked
    | PathsRequested Int String
    | SettingsBoardNameClicked Int
    | ToggleIncludeOthers
    | ToggleIncludeUndated
    | ToggleIncludeUntagged


update : Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update msg model =
    case msg of
        AddBoardClicked ->
            wrap { model | settingsState = SettingsState.addState }

        AddBoardConfirmed ->
            processsAction (SettingsState.confirmAdd model.settingsState) model

        BoardTypeSelected boardType ->
            updateBoardBeingAdded (BoardConfig.updateBoardType boardType) model

        DeleteBoardRequested ->
            wrap { model | settingsState = SettingsState.deleteState }

        DeleteBoardConfirmed ->
            processsAction (SettingsState.confirmDelete model.settingsState) model

        EnteredCompletedCount value ->
            updateBoardBeingEdited (BoardConfig.updateCompletedCount (String.toInt value)) model

        EnteredNewBoardTitle title ->
            updateBoardBeingAdded (BoardConfig.updateTitle title) model

        EnteredTags tags ->
            updateBoardBeingEdited (BoardConfig.updateTags tags) model

        EnteredTitle title ->
            updateBoardBeingEdited (BoardConfig.updateTitle title) model

        FolderPathsReceived folderPaths ->
            let
                _ =
                    Debug.log "Received paths" folderPaths
            in
            wrap { model | pathCache = State.Loaded folderPaths }

        GotMultiSelectMsg mulSelMsg ->
            let
                ( newModel, cmd ) =
                    MultiSelect.update mulSelMsg model.multiSelect
            in
            ( { model | multiSelect = newModel }
            , cmd
            , Session.NoOp
            )

        ModalCancelClicked ->
            processsAction (SettingsState.cancelCurrentState model.settingsState) model

        ModalCloseClicked ->
            processsAction (SettingsState.cancelCurrentState model.settingsState) model

        PathsRequested page searchTerm ->
            let
                cmd =
                    case model.pathCache of
                        State.Waiting ->
                            InteropPorts.requestPaths

                        State.Loading paths ->
                            Cmd.none

                        State.Loaded paths ->
                            Cmd.none
            in
            ( model
            , cmd
            , Session.NoOp
            )

        SettingsBoardNameClicked index ->
            wrap { model | boardConfigs = SafeZipper.atIndex index model.boardConfigs }

        ToggleIncludeOthers ->
            updateBoardBeingEdited BoardConfig.toggleIncludeOthers model

        ToggleIncludeUndated ->
            updateBoardBeingEdited BoardConfig.toggleIncludeUndated model

        ToggleIncludeUntagged ->
            updateBoardBeingEdited BoardConfig.toggleIncludeUntagged model


updateBoardBeingAdded : (BoardConfig -> BoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
updateBoardBeingAdded fn model =
    wrap { model | settingsState = SettingsState.mapBoardBeingAdded fn model.settingsState }


updateBoardBeingEdited : (BoardConfig -> BoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
updateBoardBeingEdited fn model =
    wrap { model | boardConfigs = SafeZipper.mapCurrent fn model.boardConfigs }


processsAction : SettingsState.Action -> Model -> ( Model, Cmd Msg, Session.Msg )
processsAction action model =
    case action of
        SettingsState.AddBoard newConfig newState ->
            let
                newConfigs =
                    SafeZipper.add newConfig model.boardConfigs
                        |> SafeZipper.last
            in
            wrap { model | boardConfigs = newConfigs, settingsState = newState }

        SettingsState.AddCancelled newState ->
            let
                sessionMsg =
                    if SafeZipper.length model.boardConfigs == 0 then
                        Session.SettingsClosed model.boardConfigs

                    else
                        Session.NoOp
            in
            ( { model | settingsState = newState }
            , exitCmdOr Cmd.none model.boardConfigs
            , sessionMsg
            )

        SettingsState.DeleteCurrent ->
            let
                newConfigs =
                    SafeZipper.deleteCurrent model.boardConfigs
            in
            wrap
                { model
                    | boardConfigs = newConfigs
                    , settingsState = SettingsState.init newConfigs
                }

        SettingsState.SetToState newState ->
            wrap { model | settingsState = newState }

        SettingsState.Exit ->
            ( model
            , exitCmdOr (InteropPorts.updateSettings model.boardConfigs) model.boardConfigs
            , Session.SettingsClosed model.boardConfigs
            )

        SettingsState.NoAction ->
            wrap model


exitCmdOr : Cmd Msg -> SafeZipper BoardConfig -> Cmd Msg
exitCmdOr orCmd boardConfigs =
    if SafeZipper.length boardConfigs == 0 then
        Cmd.batch
            [ InteropPorts.updateSettings boardConfigs
            , InteropPorts.closeView
            ]

    else
        orCmd


wrap : Model -> ( Model, Cmd Msg, Session.Msg )
wrap model =
    ( model, Cmd.none, Session.NoOp )



-- VIEW


view : Model -> Html Msg
view model =
    case model.settingsState of
        SettingsState.AddingBoard newConfig ->
            Html.div []
                [ modalSettingsView model.boardConfigs model.multiSelect
                , modalAddBoard newConfig
                ]

        SettingsState.DeletingBoard ->
            Html.div []
                [ modalSettingsView model.boardConfigs model.multiSelect
                , modalConfirmDelete
                ]

        SettingsState.EditingBoard ->
            modalSettingsView model.boardConfigs model.multiSelect


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


modalSettingsView : SafeZipper BoardConfig -> MultiSelect.Model Msg String -> Html Msg
modalSettingsView configs multiselect =
    Html.div [ class "modal-container" ]
        [ Html.div [ class "modal-bg" ] []
        , Html.div [ class "modal mod-settings" ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-content" ]
                [ Html.div [ class "settings-menu vertical-tab-header" ]
                    (Html.div [ class "vertical-tab-header-group-title" ]
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
                        :: (configs
                                |> SafeZipper.indexedMapSelectedAndRest settingTitleSelectedView settingTitleView
                                |> SafeZipper.toList
                           )
                    )
                , settingsFormView (SafeZipper.current configs) multiselect
                ]
            ]
        ]


settingsFormView : Maybe BoardConfig -> MultiSelect.Model Msg String -> Html Msg
settingsFormView boardConfig multiselect =
    case boardConfig of
        Just (BoardConfig.DateBoardConfig config) ->
            let
                includeUndatedStyle =
                    if config.includeUndated then
                        " is-enabled"

                    else
                        ""
            in
            Html.div [ class "settings-form-container" ]
                [ Html.div [ class "settings-form" ]
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
                    , Html.div [ class "setting-item" ]
                        [ Html.div [ class "setting-item-info" ]
                            [ Html.div [ class "setting-item-name" ]
                                [ Html.text "Filters" ]
                            , Html.div [ class "setting-item-description" ]
                                [ Html.text "Limit the board to the chosen paths" ]
                            ]
                        , Html.div [ class "setting-item-control" ]
                            [ MultiSelect.view multiselect
                            ]
                        ]
                    , Html.div [ class "setting-item" ]
                        [ Html.div [ class "setting-item-info" ]
                            [ Html.div [ class "setting-item-name" ]
                                [ Html.text "Include Undated" ]
                            , Html.div [ class "setting-item-description" ]
                                [ Html.text "Whether to include a column for tasks with no due date" ]
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
                                [ Html.text "Completed Count" ]
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

        Just (BoardConfig.TagBoardConfig config) ->
            let
                includeOthersStyle =
                    if config.includeOthers then
                        " is-enabled"

                    else
                        ""

                includeUntaggedStyle =
                    if config.includeUntagged then
                        " is-enabled"

                    else
                        ""

                tagText =
                    config.columns
                        |> List.map (\c -> "#" ++ c.tag ++ " " ++ c.displayTitle)
                        |> String.join "\n"
            in
            Html.div [ class "settings-form-container" ]
                [ Html.div [ class "settings-form" ]
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
                    , Html.div [ class "setting-item" ]
                        [ Html.div [ class "setting-item-info" ]
                            [ Html.div [ class "setting-item-name" ]
                                [ Html.text "Columns" ]
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
                        , Html.div [ class "setting-item-control" ]
                            [ Html.textarea
                                [ onInput EnteredTags
                                , placeholder "#tag1 Column heading\n#tag2/\n#tag3/subtag\ntag4"
                                ]
                                [ Html.text tagText ]
                            ]
                        ]
                    , Html.div [ class "setting-item" ]
                        [ Html.div [ class "setting-item-info" ]
                            [ Html.div [ class "setting-item-name" ]
                                [ Html.text "Include Others" ]
                            , Html.div [ class "setting-item-description" ]
                                [ Html.text "Whether to include a column for tasks with tags other than those specified" ]
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
                                [ Html.text "Include Untagged" ]
                            , Html.div [ class "setting-item-description" ]
                                [ Html.text "Whether to include a column for tasks with no tags" ]
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
                                [ Html.text "Completed Count" ]
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

        Nothing ->
            Html.text ""


settingTitleSelectedView : Int -> BoardConfig -> Html Msg
settingTitleSelectedView index boardConfig =
    Html.div
        [ class "vertical-tab-nav-item is-active"
        , onClick <| SettingsBoardNameClicked index
        ]
        [ Html.text <| BoardConfig.title boardConfig ]


settingTitleView : Int -> BoardConfig -> Html Msg
settingTitleView index boardConfig =
    Html.div
        [ class "vertical-tab-nav-item"
        , onClick <| SettingsBoardNameClicked index
        ]
        [ Html.text <| BoardConfig.title boardConfig ]
