module Page.Settings exposing
    ( Msg(..)
    , dialogs
    , update
    )

import BoardConfig exposing (BoardConfig)
import DateBoard
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import InteropPorts
import Model exposing (EditState(..), Model)
import Parser
import SafeZipper exposing (SafeZipper)
import TagBoard



-- UPDATE


type Msg
    = AddBoardClicked
    | BoardTypeSelected String
    | DeleteBoardRequested
    | DeleteBoardConfirmed
    | EnteredCompletedCount String
    | EnteredNewBoardTitle String
    | EnteredTags String
    | EnteredTitle String
    | ModalCancelClicked
    | ModalCloseClicked
    | NewBoardDetailsEntered
    | SettingsBoardNameClicked Int
    | ToggleIncludeOthers
    | ToggleIncludeUndated
    | ToggleIncludeUntagged


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddBoardClicked ->
            let
                newConfig : EditState
                newConfig =
                    case model.configBeingEdited of
                        Editing c ->
                            Adding c BoardConfig.default

                        _ ->
                            model.configBeingEdited
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        BoardTypeSelected board ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Adding cs c ->
                            Model.Adding cs (updateBoardType c)

                        _ ->
                            model.configBeingEdited

                updateBoardType : BoardConfig -> BoardConfig
                updateBoardType config =
                    case board of
                        "dateBoard" ->
                            let
                                newBoardConfig =
                                    DateBoard.defaultConfig
                            in
                            BoardConfig.DateBoardConfig { newBoardConfig | title = BoardConfig.title config }

                        _ ->
                            let
                                newBoardConfig =
                                    TagBoard.defaultConfig
                            in
                            BoardConfig.TagBoardConfig { newBoardConfig | title = BoardConfig.title config }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        DeleteBoardRequested ->
            case model.configBeingEdited of
                Model.Editing c ->
                    ( { model | configBeingEdited = Model.Deleting c }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DeleteBoardConfirmed ->
            case model.configBeingEdited of
                Model.Deleting c ->
                    let
                        newConfig =
                            SafeZipper.deleteCurrent c
                    in
                    ( { model | configBeingEdited = Model.Editing newConfig }
                        |> Model.forceAddWhenNoBoards newConfig
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EnteredCompletedCount value ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Editing c ->
                            Model.Editing (SafeZipper.mapCurrent updateCompletedCount c)

                        _ ->
                            model.configBeingEdited

                updateCompletedCount : BoardConfig -> BoardConfig
                updateCompletedCount config =
                    case ( config, String.toInt value ) of
                        ( BoardConfig.DateBoardConfig dateBoardConfig, Just newCount ) ->
                            BoardConfig.DateBoardConfig { dateBoardConfig | completedCount = newCount }

                        ( BoardConfig.TagBoardConfig tagBoardConfig, Just newCount ) ->
                            BoardConfig.TagBoardConfig { tagBoardConfig | completedCount = newCount }

                        _ ->
                            config
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        EnteredNewBoardTitle title ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Adding cs c ->
                            Model.Adding cs (updateTitle c)

                        _ ->
                            model.configBeingEdited

                updateTitle : BoardConfig -> BoardConfig
                updateTitle config =
                    case config of
                        BoardConfig.DateBoardConfig dateBoardConfig ->
                            BoardConfig.DateBoardConfig { dateBoardConfig | title = title }

                        BoardConfig.TagBoardConfig tagBoardConfig ->
                            BoardConfig.TagBoardConfig { tagBoardConfig | title = title }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        EnteredTags tags ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Editing c ->
                            Model.Editing (SafeZipper.mapCurrent updateTags c)

                        _ ->
                            model.configBeingEdited

                updateTags : BoardConfig -> BoardConfig
                updateTags config =
                    case config of
                        BoardConfig.DateBoardConfig _ ->
                            config

                        BoardConfig.TagBoardConfig tagBoardConfig ->
                            let
                                columnsConfig =
                                    Parser.run TagBoard.columnConfigsParser tags
                            in
                            case columnsConfig of
                                Ok parsedConfig ->
                                    BoardConfig.TagBoardConfig { tagBoardConfig | columns = parsedConfig }

                                _ ->
                                    config
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        EnteredTitle title ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Editing c ->
                            Model.Editing (SafeZipper.mapCurrent updateTitle c)

                        _ ->
                            model.configBeingEdited

                updateTitle : BoardConfig -> BoardConfig
                updateTitle config =
                    case config of
                        BoardConfig.DateBoardConfig dateBoardConfig ->
                            BoardConfig.DateBoardConfig { dateBoardConfig | title = title }

                        BoardConfig.TagBoardConfig tagBoardConfig ->
                            BoardConfig.TagBoardConfig { tagBoardConfig | title = title }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ModalCancelClicked ->
            closeDialogOrExit model

        ModalCloseClicked ->
            closeDialogOrExit model

        NewBoardDetailsEntered ->
            case model.configBeingEdited of
                Model.Adding config newConfig ->
                    let
                        configWithNew =
                            SafeZipper.add newConfig config
                                |> SafeZipper.last
                    in
                    ( { model | configBeingEdited = Model.Editing configWithNew }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SettingsBoardNameClicked index ->
            case model.configBeingEdited of
                Model.Editing boardConfigs ->
                    ( { model | configBeingEdited = Model.Editing <| SafeZipper.atIndex index boardConfigs }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleIncludeOthers ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Editing c ->
                            Model.Editing (SafeZipper.mapCurrent toggleIncludeOthers c)

                        _ ->
                            model.configBeingEdited

                toggleIncludeOthers : BoardConfig -> BoardConfig
                toggleIncludeOthers config =
                    case config of
                        BoardConfig.DateBoardConfig _ ->
                            config

                        BoardConfig.TagBoardConfig tagBoardConfig ->
                            BoardConfig.TagBoardConfig { tagBoardConfig | includeOthers = not tagBoardConfig.includeOthers }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ToggleIncludeUndated ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Editing c ->
                            Model.Editing (SafeZipper.mapCurrent toggleIncludeUndated c)

                        _ ->
                            model.configBeingEdited

                toggleIncludeUndated : BoardConfig -> BoardConfig
                toggleIncludeUndated config =
                    case config of
                        BoardConfig.DateBoardConfig dateBoardConfig ->
                            BoardConfig.DateBoardConfig { dateBoardConfig | includeUndated = not dateBoardConfig.includeUndated }

                        BoardConfig.TagBoardConfig _ ->
                            config
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ToggleIncludeUntagged ->
            let
                newConfig : Model.EditState
                newConfig =
                    case model.configBeingEdited of
                        Model.Editing c ->
                            Model.Editing (SafeZipper.mapCurrent toggleIncludeUntagged c)

                        _ ->
                            model.configBeingEdited

                toggleIncludeUntagged : BoardConfig -> BoardConfig
                toggleIncludeUntagged config =
                    case config of
                        BoardConfig.DateBoardConfig _ ->
                            config

                        BoardConfig.TagBoardConfig tagBoardConfig ->
                            BoardConfig.TagBoardConfig { tagBoardConfig | includeUntagged = not tagBoardConfig.includeUntagged }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )


closeDialogOrExit : Model -> ( Model, Cmd Msg )
closeDialogOrExit model =
    case model.configBeingEdited of
        Model.Adding config _ ->
            let
                cmd =
                    if SafeZipper.length config == 0 then
                        Cmd.batch
                            [ InteropPorts.updateSettings config
                            , InteropPorts.closeView
                            ]

                    else
                        Cmd.none
            in
            ( { model | configBeingEdited = Model.Editing config }
                |> Model.forceAddWhenNoBoards config
            , cmd
            )

        Model.Deleting config ->
            ( { model | configBeingEdited = Model.Editing config }
            , Cmd.none
            )

        Model.Editing config ->
            ( { model | configBeingEdited = Model.NotEditing }
            , InteropPorts.updateSettings config
            )

        _ ->
            ( { model | configBeingEdited = Model.NotEditing }
            , Cmd.none
            )



-- VIEW


dialogs : Model.EditState -> Html Msg
dialogs editState =
    case editState of
        Model.Adding configsBeingEdited newConfig ->
            Html.div []
                [ modalSettingsView configsBeingEdited
                , modalAddBoard newConfig
                ]

        Model.Deleting configsBeingEdited ->
            Html.div []
                [ modalSettingsView configsBeingEdited
                , modalConfirmDelete
                ]

        Model.Editing configsBeingEdited ->
            modalSettingsView configsBeingEdited

        Model.NotEditing ->
            Html.text ""


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
                        , onClick <| NewBoardDetailsEntered
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
                                [ Html.text "Whether to include a colum for tasks with no due date" ]
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
                                [ Html.text "Whether to include a colum for tasks with tags other than those specified" ]
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
                                [ Html.text "Whether to include a colum for tasks with no tags" ]
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
