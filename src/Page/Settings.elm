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

import BoardConfig exposing (BoardConfig)
import FeatherIcons
import Flip
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import InteropPorts
import Parser
import SafeZipper exposing (SafeZipper)
import Session exposing (Session)
import SettingsState exposing (SettingsState)
import TagBoard



-- MODEL


type alias Model =
    { session : Session
    , settingsState : SettingsState
    }


init : Session -> Model
init session =
    let
        settingsState =
            if SafeZipper.length (Session.boardConfigs session) == 0 then
                SettingsState.forNoConfig

            else
                SettingsState.startEditing (Session.boardConfigs session)
    in
    { session = session
    , settingsState = settingsState
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


mapSetingsEditState : (SettingsState -> SettingsState) -> Model -> Model
mapSetingsEditState fn model =
    { model | settingsState = fn model.settingsState }


mapBoardBeingAdded : (BoardConfig -> BoardConfig) -> Model -> Model
mapBoardBeingAdded fn model =
    { model | settingsState = SettingsState.mapBoardBeingAdded fn model.settingsState }


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> Model -> Model
mapBoardBeingEdited fn model =
    { model | settingsState = SettingsState.mapBoardBeingEdited fn model.settingsState }



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
    | ModalCancelClicked
    | ModalCloseClicked
    | SettingsBoardNameClicked Int
    | ToggleIncludeOthers
    | ToggleIncludeUndated
    | ToggleIncludeUntagged


update : Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update msg model =
    case msg of
        AddBoardClicked ->
            ( mapSetingsEditState SettingsState.moveToAdd model
            , Cmd.none
            , Session.NoOp
            )

        AddBoardConfirmed ->
            ( mapSetingsEditState SettingsState.confirmAdd model
            , Cmd.none
            , Session.NoOp
            )

        BoardTypeSelected boardType ->
            ( mapBoardBeingAdded (BoardConfig.updateBoardType boardType) model
            , Cmd.none
            , Session.NoOp
            )

        DeleteBoardRequested ->
            ( mapSetingsEditState SettingsState.moveToDelete model
            , Cmd.none
            , Session.NoOp
            )

        DeleteBoardConfirmed ->
            ( mapSetingsEditState SettingsState.confirmDelete model
            , Cmd.none
            , Session.NoOp
            )

        EnteredCompletedCount value ->
            ( mapBoardBeingEdited (BoardConfig.updateCompletedCount (String.toInt value)) model
            , Cmd.none
            , Session.NoOp
            )

        EnteredNewBoardTitle title ->
            ( mapBoardBeingAdded (BoardConfig.updateTitle title) model
            , Cmd.none
            , Session.NoOp
            )

        EnteredTags tags ->
            ( mapBoardBeingEdited (BoardConfig.updateTags tags) model
            , Cmd.none
            , Session.NoOp
            )

        EnteredTitle title ->
            ( mapBoardBeingEdited (BoardConfig.updateTitle title) model
            , Cmd.none
            , Session.NoOp
            )

        ModalCancelClicked ->
            closeDialogOrExit model

        ModalCloseClicked ->
            closeDialogOrExit model

        SettingsBoardNameClicked index ->
            ( mapSetingsEditState (SettingsState.changeBoardBeingEdited index) model
            , Cmd.none
            , Session.NoOp
            )

        ToggleIncludeOthers ->
            ( mapBoardBeingEdited BoardConfig.toggleIncludeOthers model
            , Cmd.none
            , Session.NoOp
            )

        ToggleIncludeUndated ->
            ( mapBoardBeingEdited BoardConfig.toggleIncludeUndated model
            , Cmd.none
            , Session.NoOp
            )

        ToggleIncludeUntagged ->
            ( mapBoardBeingEdited BoardConfig.toggleIncludeUntagged model
            , Cmd.none
            , Session.NoOp
            )


closeDialogOrExit : Model -> ( Model, Cmd Msg, Session.Msg )
closeDialogOrExit model =
    let
        ( newState, cancelAction ) =
            SettingsState.cancelCurrentState model.settingsState
    in
    case cancelAction of
        SettingsState.ExitWithConfig config ->
            ( mapSessionConfig (\c -> { c | boardConfigs = config }) model
            , InteropPorts.updateSettings config
            , Session.SettingsClosed
            )

        SettingsState.ExitWithNoConfig ->
            ( mapSessionConfig (\c -> { c | boardConfigs = SafeZipper.empty }) model
            , Cmd.batch
                [ InteropPorts.updateSettings SafeZipper.empty
                , InteropPorts.closeView
                ]
            , Session.SettingsClosed
            )

        SettingsState.SetToState ->
            ( mapSetingsEditState (always newState) model
            , Cmd.none
            , Session.NoOp
            )



-- VIEW


view : Model -> Html Msg
view model =
    case model.settingsState of
        SettingsState.AddingBoard configsBeingEdited newConfig ->
            Html.div []
                [ modalSettingsView configsBeingEdited
                , modalAddBoard newConfig
                ]

        SettingsState.DeletingBoard configsBeingEdited ->
            Html.div []
                [ modalSettingsView configsBeingEdited
                , modalConfirmDelete
                ]

        SettingsState.EditingBoard configsBeingEdited ->
            modalSettingsView configsBeingEdited


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


modalSettingsView : SafeZipper BoardConfig -> Html Msg
modalSettingsView configs =
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
                , settingsFormView <| SafeZipper.current configs
                ]
            ]
        ]


settingsFormView : Maybe BoardConfig -> Html Msg
settingsFormView boardConfig =
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
