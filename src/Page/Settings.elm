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
import Filter exposing (Filter, Polarity)
import Html exposing (Html)
import Html.Attributes exposing (class, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import InteropPorts
import List.Extra as LE
import Page.Helper.Multiselect as MultiSelect
import SafeZipper exposing (SafeZipper)
import Session exposing (Session)
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
    let
        boardConfigs : SafeZipper BoardConfig
        boardConfigs =
            Session.boardConfigs session
    in
    { multiSelect = MultiSelect.init multiSelectConfig (currentFilters boardConfigs)
    , pathCache = State.Waiting
    , session = session
    , settingsState = SettingsState.init boardConfigs
    }


currentFilters : SafeZipper BoardConfig -> Dict String Filter
currentFilters boardConfigs =
    SafeZipper.current boardConfigs
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


mapSessionConfig : (Session.Config -> Session.Config) -> Model -> Model
mapSessionConfig fn model =
    { model | session = Session.mapConfig fn model.session }


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
    | GotMultiSelectMsg (MultiSelect.Msg Msg Filter)
    | ModalCancelClicked
    | ModalCloseClicked
    | PathsRequested Int String
    | PolaritySelected String
    | ToggleIncludeOthers
    | ToggleIncludeUndated
    | ToggleIncludeUntagged
    | ToggleShowColumnTags
    | ToggleShowFilteredTags


update : Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update msg model =
    case msg of
        AddBoardClicked ->
            wrap { model | settingsState = SettingsState.addBoardRequested model.settingsState }

        AddBoardConfirmed ->
            wrap { model | settingsState = SettingsState.confirmAddBoard model.settingsState }

        BackspacePressed ->
            wrap { model | multiSelect = MultiSelect.deleteHighlightedItem model.multiSelect }

        BoardNameClicked index ->
            let
                currentSelectedFilters =
                    Dict.values <| MultiSelect.selectedItems model.multiSelect

                newSettingsState =
                    model.settingsState
                        |> SettingsState.updateBoardBeingEdited (BoardConfig.updateFilters currentSelectedFilters)
                        |> SettingsState.switchBoardBeingEdited index

                newFilters =
                    currentFilters <| SettingsState.boardConfigs newSettingsState
            in
            wrap
                { model
                    | settingsState = newSettingsState
                    , multiSelect = MultiSelect.updateSelectedItems newFilters model.multiSelect
                }

        BoardTypeSelected boardType ->
            updateBoardBeingAdded (BoardConfig.updateBoardType boardType) model

        DeleteBoardRequested ->
            wrap { model | settingsState = SettingsState.deleteBoardRequested model.settingsState }

        DeleteBoardConfirmed ->
            wrap { model | settingsState = SettingsState.confirmDeleteBoard model.settingsState }

        EnteredCompletedCount value ->
            updateBoardBeingEdited (BoardConfig.updateCompletedCount (String.toInt value)) model

        EnteredNewBoardTitle title ->
            updateBoardBeingAdded (BoardConfig.updateTitle title) model

        EnteredTags tags ->
            updateBoardBeingEdited (BoardConfig.updateTags tags) model

        EnteredTitle title ->
            updateBoardBeingEdited (BoardConfig.updateTitle title) model

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
            updateBoardBeingEdited (BoardConfig.updateFilterPolarity polarity) model

        ToggleIncludeOthers ->
            updateBoardBeingEdited BoardConfig.toggleIncludeOthers model

        ToggleIncludeUndated ->
            updateBoardBeingEdited BoardConfig.toggleIncludeUndated model

        ToggleIncludeUntagged ->
            updateBoardBeingEdited BoardConfig.toggleIncludeUntagged model

        ToggleShowColumnTags ->
            updateBoardBeingEdited BoardConfig.toggleShowColumnTags model

        ToggleShowFilteredTags ->
            updateBoardBeingEdited BoardConfig.toggleShowFilteredTags model


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


updateBoardBeingAdded : (BoardConfig -> BoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
updateBoardBeingAdded fn model =
    wrap { model | settingsState = SettingsState.updateBoardBeingAdded fn model.settingsState }


updateBoardBeingEdited : (BoardConfig -> BoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
updateBoardBeingEdited fn model =
    wrap { model | settingsState = SettingsState.updateBoardBeingEdited fn model.settingsState }


handleClose : Model -> ( Model, Cmd Msg, Session.Msg )
handleClose model =
    let
        newSettingsState =
            SettingsState.cancelCurrentState model.settingsState

        newBoardConfigs =
            SettingsState.boardConfigs newSettingsState
    in
    case newSettingsState of
        SettingsState.ClosingPlugin cs ->
            ( { model | settingsState = newSettingsState }
            , Cmd.batch
                [ InteropPorts.updateSettings newBoardConfigs
                , InteropPorts.closeView
                ]
            , Session.SettingsClosed newBoardConfigs
            )

        SettingsState.ClosingSettings cs ->
            ( { model | settingsState = newSettingsState }
            , InteropPorts.updateSettings newBoardConfigs
            , Session.SettingsClosed newBoardConfigs
            )

        _ ->
            wrap { model | settingsState = newSettingsState }


wrap : Model -> ( Model, Cmd Msg, Session.Msg )
wrap model =
    ( model, Cmd.none, Session.NoOp )



-- VIEW


view : Model -> Html Msg
view model =
    case model.settingsState of
        SettingsState.AddingBoard newConfig allConfigs ->
            Html.div []
                [ modalSettingsView allConfigs model.multiSelect
                , modalAddBoard newConfig
                ]

        SettingsState.ClosingPlugin allConfigs ->
            Html.text ""

        SettingsState.ClosingSettings allConfigs ->
            Html.text ""

        SettingsState.DeletingBoard allConfigs ->
            Html.div []
                [ modalSettingsView allConfigs model.multiSelect
                , modalConfirmDelete
                ]

        SettingsState.EditingBoard allConfigs ->
            modalSettingsView allConfigs model.multiSelect


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


modalSettingsView : SafeZipper BoardConfig -> MultiSelect.Model Msg Filter -> Html Msg
modalSettingsView configs multiselect =
    Html.div [ class "modal-container" ]
        [ Html.div [ class "modal-bg" ] []
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
                            [ Html.div [ class "vertical-tab-nav-item" ]
                                [ Html.text "Task format" ]
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
                                |> SafeZipper.indexedMapSelectedAndRest settingTitleSelectedView settingTitleView
                                |> SafeZipper.toList
                            )
                        ]
                    ]
                , settingsFormView (SafeZipper.current configs) (SafeZipper.currentIndex configs) multiselect
                ]
            ]
        ]


settingsFormView : Maybe BoardConfig -> Maybe Int -> MultiSelect.Model Msg Filter -> Html Msg
settingsFormView boardConfig boardIndex multiselect =
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
            Html.div [ class "vertical-tab-content-container" ]
                [ Html.div [ class "vertical-tab-content" ]
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
            Html.div [ class "vertical-tab-content-container" ]
                [ Html.div [ class "vertical-tab-content" ]
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
                ]

        _ ->
            Html.text ""


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
