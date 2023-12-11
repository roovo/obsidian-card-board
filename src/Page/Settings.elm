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
import Column exposing (Column)
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn exposing (RelativeDateRange)
import Column.NamedTag as NamedTagColumn
import Columns
import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords
import DragAndDrop.DragData as DragData exposing (DragData)
import DragAndDrop.DragTracker as DragTracker exposing (DragTracker)
import DragAndDrop.Rect as Rect
import FeatherIcons exposing (Icon)
import Filter exposing (Filter, Polarity)
import GlobalSettings exposing (GlobalSettings, TaskCompletionFormat)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, id, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Mouse exposing (onDown)
import InteropPorts
import Json.Encode as JE
import List.Extra as LE
import NewBoardConfig exposing (NewBoardConfig)
import NewColumnConfig exposing (NewColumnConfig)
import Page.Helper.Multiselect as MultiSelect
import SafeZipper exposing (SafeZipper)
import Session exposing (Session)
import Settings exposing (Settings)
import SettingsState exposing (SettingsState)
import State exposing (State)
import Svg
import Svg.Attributes as Svg



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
    | AddColumnClicked
    | AddColumnConfirmed
    | BackspacePressed
    | BoardNameClicked Int
    | BoardNameMouseDown ( String, DragTracker.ClientData )
    | ColumnDeleteClicked Int
    | DeleteBoardRequested
    | DeleteConfirmed
    | ElementDragged DragData
    | EnteredColumnName Int String
    | EnteredColumnNamedTagTag Int String
    | EnteredDatedColumnRangeValueFrom Int String
    | EnteredDatedColumnRangeValueTo Int String
    | EnteredDefaultColumnName String String
    | EnteredNewBoardName String
    | EnteredNewColumnName String
    | EnteredNewColumnCompletedLimit Int String
    | EnteredName String
    | FilterCandidatesReceived (List Filter)
    | GlobalSettingsClicked
    | GotMultiSelectMsg (MultiSelect.Msg Msg Filter)
    | ModalCancelClicked
    | ModalCloseClicked
    | NewBoardTypeSelected String
    | NewColumnTypeSelected String
    | PathsRequested Int String
    | PolaritySelected String
    | SelectedDatedColumnRangeType Int String
    | TaskCompletionFormatSelected String
    | ToggleIgnoreFileNameDate
    | ToggleShowColumnTags
    | ToggleShowFilteredTags
    | ToggleTagFilterScope


dragType : String
dragType =
    "card-board-settings-board-name"


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
            let
                defaultColumnNames : DefaultColumnNames
                defaultColumnNames =
                    model
                        |> toSession
                        |> Session.settings
                        |> Settings.globalSettings
                        |> .defaultColumnNames
            in
            wrap <|
                switchSettingsState
                    (SettingsState.confirmAddBoard defaultColumnNames)
                    model

        AddColumnClicked ->
            mapSettingsState SettingsState.addColumnRequested model

        AddColumnConfirmed ->
            let
                defaultColumnNames : DefaultColumnNames
                defaultColumnNames =
                    model
                        |> toSession
                        |> Session.settings
                        |> Settings.globalSettings
                        |> .defaultColumnNames
            in
            wrap <|
                switchSettingsState
                    (SettingsState.confirmAddColumn defaultColumnNames)
                    model

        BackspacePressed ->
            wrap { model | multiSelect = MultiSelect.deleteHighlightedItem model.multiSelect }

        BoardNameClicked index ->
            wrap <| switchSettingsState (SettingsState.editBoardAt index) model

        BoardNameMouseDown ( domId, clientData ) ->
            ( { model | session = Session.waitForDrag clientData model.session }
            , InteropPorts.trackDraggable dragType clientData.clientPos domId
            , Session.NoOp
            )

        ColumnDeleteClicked index ->
            mapSettingsState (SettingsState.deleteColumnRequested index) model

        DeleteBoardRequested ->
            mapSettingsState SettingsState.deleteBoardRequested model

        DeleteConfirmed ->
            wrap <| switchSettingsState SettingsState.confirmDelete model

        ElementDragged dragData ->
            case dragData.dragAction of
                DragData.Move ->
                    if dragData.dragType == dragType then
                        ( model
                            |> updateBoardOrder (Session.dragTracker model.session) dragData
                            |> (\m -> { m | session = Session.moveDragable dragData m.session })
                        , Cmd.none
                        , Session.NoOp
                        )

                    else
                        ( model, Cmd.none, Session.NoOp )

                DragData.Stop ->
                    ( { model | session = Session.stopTrackingDragable model.session }
                    , Cmd.none
                    , Session.NoOp
                    )

        EnteredColumnName columnIndex name ->
            mapBoardBeingEdited (BoardConfig.updateColumnName columnIndex name) model

        EnteredColumnNamedTagTag columnIndex tag ->
            mapBoardBeingEdited (BoardConfig.updateNamedTagTag columnIndex tag) model

        EnteredDatedColumnRangeValueFrom index value ->
            mapBoardBeingEdited (BoardConfig.updateDatedColumnRangeValueFrom index (String.toInt value)) model

        EnteredDatedColumnRangeValueTo index value ->
            mapBoardBeingEdited (BoardConfig.updateDatedColumnRangeValueTo index (String.toInt value)) model

        EnteredDefaultColumnName column name ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            (GlobalSettings.updateColumnName column name)
                            model.settingsState
                }

        EnteredNewBoardName name ->
            mapBoardBeingAdded (NewBoardConfig.updateName name) model

        EnteredNewColumnName name ->
            mapColumnBeingAdded (NewColumnConfig.updateName name) model

        EnteredNewColumnCompletedLimit columnIndex limit ->
            mapBoardBeingEdited (BoardConfig.updateCompletedColumnLimit columnIndex (String.toInt limit)) model

        EnteredName name ->
            mapBoardBeingEdited (BoardConfig.updateName name) model

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

        NewBoardTypeSelected boardType ->
            mapBoardBeingAdded (NewBoardConfig.updateBoardType boardType) model

        NewColumnTypeSelected columnType ->
            mapColumnBeingAdded (NewColumnConfig.updateColumnType columnType) model

        PathsRequested _ _ ->
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

        SelectedDatedColumnRangeType index rangeType ->
            mapBoardBeingEdited (BoardConfig.updateDatedColumnRangeType index rangeType) model

        TaskCompletionFormatSelected taskCompletionFormat ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            (GlobalSettings.updateTaskCompletionFormat taskCompletionFormat)
                            model.settingsState
                }

        ToggleIgnoreFileNameDate ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            GlobalSettings.toggleIgnoreFileNameDate
                            model.settingsState
                }

        ToggleShowColumnTags ->
            mapBoardBeingEdited BoardConfig.toggleShowColumnTags model

        ToggleShowFilteredTags ->
            mapBoardBeingEdited BoardConfig.toggleShowFilteredTags model

        ToggleTagFilterScope ->
            mapBoardBeingEdited BoardConfig.toggleTagFilterScope model


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


mapBoardBeingAdded : (NewBoardConfig -> NewBoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapBoardBeingAdded fn model =
    wrap { model | settingsState = SettingsState.mapBoardBeingAdded fn model.settingsState }


mapColumnBeingAdded : (NewColumnConfig -> NewColumnConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapColumnBeingAdded fn model =
    wrap { model | settingsState = SettingsState.mapColumnBeingAdded fn model.settingsState }


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapBoardBeingEdited fn model =
    wrap { model | settingsState = SettingsState.mapBoardBeingEdited fn model.settingsState }


updateBoardOrder : DragTracker -> DragData -> Model -> Model
updateBoardOrder dragTracker { cursor, beacons } model =
    case dragTracker of
        DragTracker.Dragging clientData _ ->
            case Rect.closestTo cursor beacons of
                Nothing ->
                    model

                Just position ->
                    { model
                        | settingsState =
                            SettingsState.moveBoard clientData.uniqueId position model.settingsState
                    }

        _ ->
            model


wrap : Model -> ( Model, Cmd Msg, Session.Msg )
wrap model =
    ( model, Cmd.none, Session.NoOp )



-- VIEW


type CurrentSection
    = Options
    | Boards


view : Model -> Html Msg
view model =
    let
        dragTracker : DragTracker
        dragTracker =
            Session.dragTracker model.session
    in
    case model.settingsState of
        SettingsState.AddingBoard newConfig settings ->
            Html.div []
                [ boardSettingsView (Settings.boardConfigs settings) model.multiSelect dragTracker
                , modalAddBoard newConfig
                ]

        SettingsState.AddingColumn newConfig settings ->
            Html.div []
                [ boardSettingsView (Settings.boardConfigs settings) model.multiSelect dragTracker
                , modalAddColumn newConfig settings
                ]

        SettingsState.ClosingPlugin _ ->
            Html.text ""

        SettingsState.ClosingSettings _ ->
            Html.text ""

        SettingsState.DeletingBoard settings ->
            Html.div []
                [ boardSettingsView (Settings.boardConfigs settings) model.multiSelect dragTracker
                , modalConfirmDelete
                ]

        SettingsState.DeletingColumn index settings ->
            Html.div []
                [ boardSettingsView (Settings.boardConfigs settings) model.multiSelect dragTracker
                , modalConfirmDelete
                ]

        SettingsState.EditingBoard settings ->
            boardSettingsView (Settings.boardConfigs settings)
                model.multiSelect
                dragTracker

        SettingsState.EditingGlobalSettings settings ->
            globalSettingsView (Session.dataviewTaskCompletion <| toSession model) settings dragTracker


modalAddBoard : NewBoardConfig -> Html Msg
modalAddBoard newBoardConfig =
    Html.div [ class "modal-container" ]
        [ Html.div [ class "modal-bg" ] []
        , Html.div [ class "modal" ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-title" ]
                [ Html.text "Add board" ]
            , Html.div [ class "modal-form" ]
                [ Html.div [ class "form-item" ]
                    [ Html.div [ class "form-item-name" ]
                        [ Html.text "Name" ]
                    , Html.div [ class "form-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value <| newBoardConfig.name
                            , onInput EnteredNewBoardName
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
                            , onInput NewBoardTypeSelected
                            ]
                            (NewBoardConfig.optionsForSelect newBoardConfig
                                |> List.map
                                    (\c ->
                                        Html.option
                                            [ value c.value
                                            , selected c.isSelected
                                            ]
                                            [ Html.text c.text ]
                                    )
                            )
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


modalAddColumn : NewColumnConfig -> Settings -> Html Msg
modalAddColumn newColumnConfig settings =
    Html.div [ class "modal-container" ]
        [ Html.div [ class "modal-bg" ] []
        , Html.div [ class "modal" ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-title" ]
                [ Html.text "Add column" ]
            , Html.div [ class "modal-form" ]
                [ Html.div [ class "form-item" ]
                    [ Html.div [ class "form-item-name" ]
                        [ Html.text "Name" ]
                    , Html.div [ class "form-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value <| newColumnConfig.name
                            , onInput EnteredNewColumnName
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
                            , onInput NewColumnTypeSelected
                            ]
                            (Columns.optionsForSelect
                                (settings
                                    |> Settings.boardConfigs
                                    |> SafeZipper.current
                                    |> Maybe.map BoardConfig.columns
                                    |> Maybe.withDefault Columns.empty
                                )
                                newColumnConfig
                                |> List.map
                                    (\c ->
                                        Html.option
                                            [ value c.value
                                            , selected c.isSelected
                                            ]
                                            [ Html.text c.text ]
                                    )
                            )
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
                        , onClick <| AddColumnConfirmed
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
                    , onClick <| DeleteConfirmed
                    ]
                    [ Html.text "Go ahead"
                    ]
                ]
            ]
        ]


settingsSurroundView : CurrentSection -> SafeZipper BoardConfig -> DragTracker -> List (Html Msg) -> Html Msg
settingsSurroundView currentSection configs dragTracker formContents =
    let
        boardMapFn : Int -> BoardConfig -> Html Msg
        boardMapFn =
            case currentSection of
                Options ->
                    settingNameView

                Boards ->
                    settingNameSelectedView isDragging

        globalSettingsClass : String
        globalSettingsClass =
            case currentSection of
                Options ->
                    "vertical-tab-nav-item is-active"

                Boards ->
                    "vertical-tab-nav-item"

        isDragging : Bool
        isDragging =
            DragTracker.isDragging dragTracker && draggedType == Just dragType

        draggedType : Maybe String
        draggedType =
            DragTracker.dragType dragTracker
    in
    Html.div
        [ class "modal-container"
        , attributeIf isDragging (style "cursor" "grabbing")
        ]
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
                                |> SafeZipper.indexedMapSelectedAndRest boardMapFn settingNameView
                                |> SafeZipper.toList
                                |> (\hs ->
                                        List.append hs
                                            [ settingNameDraggedView isDragging (SafeZipper.current configs) dragTracker ]
                                   )
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


boardSettingsView : SafeZipper BoardConfig -> MultiSelect.Model Msg Filter -> DragTracker -> Html Msg
boardSettingsView boardConfigs multiselect dragTracker =
    boardSettingsForm (SafeZipper.current boardConfigs) (SafeZipper.currentIndex boardConfigs) multiselect
        |> settingsSurroundView Boards boardConfigs dragTracker


globalSettingsView : DataviewTaskCompletion -> Settings -> DragTracker -> Html Msg
globalSettingsView dataviewTaskCompletion settings dragTracker =
    settings
        |> Settings.globalSettings
        |> globalSettingsForm dataviewTaskCompletion
        |> settingsSurroundView Options (Settings.boardConfigs settings) dragTracker


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

        ignoreFileNameDatesStyle : String
        ignoreFileNameDatesStyle =
            if gs.ignoreFileNameDates then
                " is-enabled"

            else
                ""
    in
    [ Html.div [ class "setting-items-inner" ]
        ([ Html.div [ class "setting-item setting-item-heading" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Daily/Periodic notes compatibility" ]
                , Html.div [ class "setting-item-description" ] []
                ]
            , Html.div [ class "setting-item-control" ] []
            ]
         , Html.div [ class "setting-item" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Ignore file name dates" ]
                , Html.div [ class "setting-item-description" ]
                    [ Html.text "If turned on tasks placed in daily note files will not use the date of the note as their due date." ]
                ]
            , Html.div [ class "setting-item-control" ]
                [ Html.div
                    [ class <| "checkbox-container" ++ ignoreFileNameDatesStyle
                    , onClick ToggleIgnoreFileNameDate
                    ]
                    []
                ]
            ]
         , Html.div [ class "setting-item setting-item-heading" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Task completion format" ]
                , Html.div [ class "setting-item-description" ] []
                ]
            , Html.div [ class "setting-item-control" ] []
            ]
         , Html.div [ class "setting-item" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-description" ]
                    [ Html.text "Format to use when marking tasks as completed:"
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
                    , Html.text " and should be reflected above."
                    , Html.i [] [ Html.text " If you change the settings in" ]
                    , Html.i [] [ Html.text " Dataview you will need to close and re-open the CardBoard view to pick up the changes." ]
                    , Html.br [] []
                    , Html.br [] []
                    , Html.text "When reading tasks, CardBoard understands all these formats."
                    ]
                ]
            , Html.div [ class "setting-item-control" ]
                [ taskCompletionFormatSelect gs.taskCompletionFormat ]
            ]
         ]
            ++ columNamesForm gs.defaultColumnNames
        )
    ]


columNamesForm : DefaultColumnNames -> List (Html Msg)
columNamesForm defaultColumnNames =
    [ Html.div [ class "setting-item setting-item-heading" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Column Names" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Customise names used for auto-generated columns." ]
            ]
        , Html.div [ class "setting-item-control" ] []
        ]
    , Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Today" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Today's tasks (Date boards)." ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ Html.input
                [ type_ "text"
                , placeholder "Today"
                , value (Maybe.withDefault "" defaultColumnNames.today)
                , onInput (EnteredDefaultColumnName "today")
                ]
                []
            ]
        ]
    , Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Tomorrow" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Tomorrow's tasks (Date boards)." ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ Html.input
                [ type_ "text"
                , placeholder "Tomorrow"
                , value (Maybe.withDefault "" defaultColumnNames.tomorrow)
                , onInput (EnteredDefaultColumnName "tomorrow")
                ]
                []
            ]
        ]
    , Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Future" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Tasks due beyond tomorrow (Date boards)." ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ Html.input
                [ type_ "text"
                , placeholder "Future"
                , value (Maybe.withDefault "" defaultColumnNames.future)
                , onInput (EnteredDefaultColumnName "future")
                ]
                []
            ]
        ]
    , Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Undated" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Undated tasks (Tag boards)." ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ Html.input
                [ type_ "text"
                , placeholder "Undated"
                , value (Maybe.withDefault "" defaultColumnNames.undated)
                , onInput (EnteredDefaultColumnName "undated")
                ]
                []
            ]
        ]
    , Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Other Tags" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Tasks with other tags (Tag boards)." ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ Html.input
                [ type_ "text"
                , placeholder "Other Tags"
                , value (Maybe.withDefault "" defaultColumnNames.otherTags)
                , onInput (EnteredDefaultColumnName "otherTags")
                ]
                []
            ]
        ]
    , Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Untagged" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Tasks with no tag (Tag boards)." ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ Html.input
                [ type_ "text"
                , placeholder "Untagged"
                , value (Maybe.withDefault "" defaultColumnNames.untagged)
                , onInput (EnteredDefaultColumnName "untagged")
                ]
                []
            ]
        ]
    , Html.div [ class "setting-item" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Completed" ]
            , Html.div [ class "setting-item-description" ]
                [ Html.text "Completed tasks." ]
            ]
        , Html.div [ class "setting-item-control" ]
            [ Html.input
                [ type_ "text"
                , placeholder "Completed"
                , value (Maybe.withDefault "" defaultColumnNames.completed)
                , onInput (EnteredDefaultColumnName "completed")
                ]
                []
            ]
        ]
    ]


boardSettingsForm : Maybe BoardConfig -> Maybe Int -> MultiSelect.Model Msg Filter -> List (Html Msg)
boardSettingsForm boardConfig boardIndex multiselect =
    case ( boardConfig, boardIndex ) of
        ( Just config, Just _ ) ->
            let
                showFilteredTagsStyle : String
                showFilteredTagsStyle =
                    if BoardConfig.showFilteredTags config then
                        " is-enabled"

                    else
                        ""

                showColumnTagsStyle : String
                showColumnTagsStyle =
                    if BoardConfig.showColumnTags config then
                        " is-enabled"

                    else
                        ""

                tagFilterScopeStyle : String
                tagFilterScopeStyle =
                    case BoardConfig.filterScope config of
                        Filter.TopLevelOnly ->
                            ""

                        Filter.SubTasksOnly ->
                            " is-mid-enabled"

                        Filter.Both ->
                            " is-enabled"

                tagFilterScopeText : String
                tagFilterScopeText =
                    case BoardConfig.filterScope config of
                        Filter.TopLevelOnly ->
                            "Top level"

                        Filter.SubTasksOnly ->
                            "Sub-tasks"

                        Filter.Both ->
                            "Both"
            in
            [ Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Name" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "The name of this board" ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.input
                        [ type_ "text"
                        , value <| BoardConfig.name config
                        , onInput EnteredName
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
                    [ polaritySelect <| BoardConfig.filterPolarity config ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Tag filter scope" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Apply tag filters to just the top level tasks, to just sub-tasks, or to both." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.div []
                        [ Html.text tagFilterScopeText ]
                    , Html.div
                        [ class <| "checkbox-container" ++ tagFilterScopeStyle
                        , onClick ToggleTagFilterScope
                        ]
                        []
                    ]
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
                [ Html.div [ class "cardboard-settings-columns-list" ]
                    (BoardConfig.columns config
                        |> Columns.toList
                        |> List.indexedMap settingsColumnView
                    )
                ]
            , Html.div [ class "setting-item-control" ]
                [ Html.button
                    [ onClick AddColumnClicked ]
                    [ Html.text "Add Column" ]
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
            , Html.div [ class "setting-item dialog-buttons" ]
                [ Html.button
                    [ class "mod-warning"
                    , onClick DeleteBoardRequested
                    ]
                    [ Html.text "Delete this board"
                    ]
                ]
            ]

        _ ->
            [ Html.text "" ]


settingsColumnView : Int -> Column -> Html Msg
settingsColumnView index column =
    Html.div [ class "cardboard-settings-column-item" ]
        [ FeatherIcons.toHtml [] dragIcon
        , Html.div [ class "cardboard-settings-column-item-detail" ]
            [ Html.text <| Column.typeString column ]
        , Html.div [ class "cardboard-settings-column-item-detail" ]
            [ Html.input
                [ type_ "text"
                , value <| Column.name column
                , onInput <| EnteredColumnName index
                ]
                []
            ]
        , settingsColumnControlView index column
        , Html.div
            [ class "card-board-card-action-area-button"
            , onClick <| ColumnDeleteClicked index
            ]
            [ FeatherIcons.trash
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        ]


settingsColumnControlView : Int -> Column -> Html Msg
settingsColumnControlView index column =
    case column of
        Column.Completed completedColumn ->
            Html.div [ class "cardboard-settings-column-item-detail" ]
                [ Html.text <| "Limit: "
                , Html.input
                    [ type_ "text"
                    , value <| String.fromInt (CompletedColumn.limit completedColumn)
                    , attribute "size" "3"
                    , onInput <| EnteredNewColumnCompletedLimit index
                    ]
                    []
                ]

        Column.Dated datedColumn ->
            Html.div [ class "cardboard-settings-column-item-detail" ]
                [ rangeSelectView index (DatedColumn.range datedColumn)
                , rangeInputsView index (DatedColumn.range datedColumn)
                ]

        Column.NamedTag namedTagColumn ->
            Html.div [ class "cardboard-settings-column-item-detail" ]
                [ Html.text <| "Tag: "
                , Html.input
                    [ type_ "text"
                    , value <| NamedTagColumn.tag namedTagColumn
                    , onInput <| EnteredColumnNamedTagTag index
                    ]
                    []
                ]

        _ ->
            Html.div [ class "cardboard-settings-column-item-detail" ]
                [ Html.text "" ]


rangeSelectView : Int -> RelativeDateRange -> Html Msg
rangeSelectView index range =
    case range of
        DatedColumn.Before _ ->
            Html.select
                [ class "dropdown"
                , onInput <| SelectedDatedColumnRangeType index
                ]
                [ Html.option [ value "Before", selected True ]
                    [ Html.text "Before" ]
                , Html.option [ value "After" ]
                    [ Html.text "After" ]
                , Html.option [ value "Between" ]
                    [ Html.text "Between" ]
                ]

        DatedColumn.After _ ->
            Html.select
                [ class "dropdown"
                , onInput <| SelectedDatedColumnRangeType index
                ]
                [ Html.option [ value "Before" ]
                    [ Html.text "Before" ]
                , Html.option [ value "After", selected True ]
                    [ Html.text "After" ]
                , Html.option [ value "Between" ]
                    [ Html.text "Between" ]
                ]

        DatedColumn.Between _ ->
            Html.select
                [ class "dropdown"
                , onInput <| SelectedDatedColumnRangeType index
                ]
                [ Html.option [ value "Before" ]
                    [ Html.text "Before" ]
                , Html.option [ value "After" ]
                    [ Html.text "After" ]
                , Html.option [ value "Between", selected True ]
                    [ Html.text "Between" ]
                ]


rangeInputsView : Int -> RelativeDateRange -> Html Msg
rangeInputsView index range =
    case range of
        DatedColumn.Before to ->
            Html.input
                [ type_ "text"
                , value <| String.fromInt to
                , attribute "size" "3"
                , onInput <| EnteredDatedColumnRangeValueTo index
                ]
                []

        DatedColumn.After from ->
            Html.input
                [ type_ "text"
                , value <| String.fromInt from
                , attribute "size" "3"
                , onInput <| EnteredDatedColumnRangeValueFrom index
                ]
                []

        DatedColumn.Between fromTo ->
            Html.div []
                [ Html.input
                    [ type_ "text"
                    , value <| String.fromInt fromTo.from
                    , attribute "size" "3"
                    , onInput <| EnteredDatedColumnRangeValueFrom index
                    ]
                    []
                , Html.span [] [ Html.text "and" ]
                , Html.input
                    [ type_ "text"
                    , value <| String.fromInt fromTo.to
                    , attribute "size" "3"
                    , onInput <| EnteredDatedColumnRangeValueTo index
                    ]
                    []
                ]


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


settingNameView : Int -> BoardConfig -> Html Msg
settingNameView index boardConfig =
    let
        domId : String
        domId =
            "card-board-setting-board-name:" ++ String.fromInt index

        name : String
        name =
            BoardConfig.name boardConfig
    in
    Html.div []
        [ beacon (BeaconPosition.Before name)
        , Html.div
            [ id domId
            , class "vertical-tab-nav-item"
            , onClick <| BoardNameClicked index
            , onDown
                (\e ->
                    BoardNameMouseDown <|
                        ( domId
                        , { uniqueId = name
                          , clientPos = Coords.fromFloatTuple e.clientPos
                          , offsetPos = Coords.fromFloatTuple e.offsetPos
                          }
                        )
                )
            ]
            [ Html.text name ]
        , beacon (BeaconPosition.After name)
        ]


settingNameSelectedView : Bool -> Int -> BoardConfig -> Html Msg
settingNameSelectedView isDragging index boardConfig =
    let
        domId : String
        domId =
            "card-board-setting-board-name:" ++ String.fromInt index

        name : String
        name =
            BoardConfig.name boardConfig
    in
    Html.div []
        [ beacon (BeaconPosition.Before name)
        , Html.div
            [ id domId
            , class "vertical-tab-nav-item is-active"
            , attributeIf isDragging (style "opacity" "0.0")
            , onClick <| BoardNameClicked index
            , onDown
                (\e ->
                    BoardNameMouseDown <|
                        ( domId
                        , { uniqueId = name
                          , clientPos = Coords.fromFloatTuple e.clientPos
                          , offsetPos = Coords.fromFloatTuple e.offsetPos
                          }
                        )
                )
            ]
            [ Html.text name ]
        , beacon (BeaconPosition.After name)
        ]


settingNameDraggedView : Bool -> Maybe BoardConfig -> DragTracker -> Html Msg
settingNameDraggedView isDragging boardConfig dragTracker =
    case ( isDragging, boardConfig, dragTracker ) of
        ( True, Just config, DragTracker.Dragging clientData domData ) ->
            Html.div []
                [ Html.div
                    [ id "card-board-settings-board-title:being-dragged"
                    , class "vertical-tab-nav-item is-active"
                    , style "position" "fixed"
                    , style "top"
                        (String.fromFloat
                            (clientData.clientPos.y - domData.offset.y - clientData.offsetPos.y)
                            ++ "px"
                        )
                    , style "left"
                        (String.fromFloat
                            (domData.draggedNodeStartRect.x - domData.offset.x)
                            ++ "px"
                        )
                    , style "width" (String.fromFloat domData.draggedNodeStartRect.width ++ "px")
                    , style "height" (String.fromFloat domData.draggedNodeStartRect.height ++ "px")
                    , style "cursor" "grabbing"
                    , style "opacity" "0.85"
                    ]
                    [ Html.text (BoardConfig.name config) ]
                ]

        _ ->
            Html.text ""


beaconType : String
beaconType =
    "data-" ++ dragType ++ "-beacon"


beacon : BeaconPosition -> Html Msg
beacon beaconPosition =
    Html.span
        [ attribute beaconType (JE.encode 0 <| BeaconPosition.encoder beaconPosition)
        , style "font-size" "0"
        ]
        []



-- HELPERS


attributeIf : Bool -> Attribute msg -> Attribute msg
attributeIf condition attribute =
    if condition then
        attribute

    else
        class ""


dragIcon : Icon
dragIcon =
    [ Svg.rect [ Svg.fill "black", Svg.x "10", Svg.y "6", Svg.width "4", Svg.height "4" ] []
    , Svg.rect [ Svg.fill "black", Svg.x "18", Svg.y "6", Svg.width "4", Svg.height "4" ] []
    , Svg.rect [ Svg.fill "black", Svg.x "10", Svg.y "14", Svg.width "4", Svg.height "4" ] []
    , Svg.rect [ Svg.fill "black", Svg.x "18", Svg.y "14", Svg.width "4", Svg.height "4" ] []
    , Svg.rect [ Svg.fill "black", Svg.x "10", Svg.y "22", Svg.width "4", Svg.height "4" ] []
    , Svg.rect [ Svg.fill "black", Svg.x "18", Svg.y "22", Svg.width "4", Svg.height "4" ] []
    ]
        |> FeatherIcons.customIcon
        |> FeatherIcons.withSize 32
        |> FeatherIcons.withViewBox "0 0 32 32"



-- |> FeatherIcons.toHtml []
