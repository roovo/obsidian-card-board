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
import DefaultColumnNames exposing (DefaultColumnNames)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords
import DragAndDrop.DragData as DragData exposing (DragData)
import DragAndDrop.DragTracker as DragTracker exposing (DragTracker)
import DragAndDrop.Rect as Rect
import FeatherIcons exposing (Icon)
import Filter exposing (Filter)
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Column as ColumnForm exposing (ColumnForm)
import Form.Column.Dated exposing (DatedColumnForm)
import Form.Columns as ColumnsForm exposing (ColumnsForm)
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.SafeDecoder as SD
import Form.Settings as SettingsForm exposing (SettingsForm)
import Form.SettingsState as SettingsState exposing (SettingsState)
import GlobalSettings exposing (TaskCompletionSettings)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, class, id, placeholder, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Mouse exposing (onDown)
import InteropPorts
import Json.Encode as JE
import List.Extra as LE
import Maybe.Extra as ME
import Page.Helper.Multiselect as MultiSelect
import SafeZipper exposing (SafeZipper)
import Session exposing (Session)
import Settings exposing (Settings)
import State exposing (State)
import Svg
import Svg.Attributes as Svg
import TimeWithZone exposing (TimeWithZone)



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
    | ColumnSettingsMouseDown ( String, DragTracker.ClientData )
    | DeleteBoardRequested
    | DeleteConfirmed
    | ElementDragged DragData
    | EnteredColumnCompletedLimit Int String
    | EnteredColumnName Int String
    | EnteredColumnNamedTagTag Int String
    | EnteredDatedColumnRangeValueFrom Int String
    | EnteredDatedColumnRangeValueTo Int String
    | EnteredDefaultColumnName String String
    | EnteredNewBoardName String
    | EnteredNewColumnName String
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
    | ScopeSelected String
    | SelectedDatedColumnRangeType Int String
    | TaskCompletionFormatSelected String
    | ToggleTaskCompletionInLocalTime
    | ToggleTaskCompletionShowUtcOffset
    | ToggleIgnoreFileNameDate
    | ToggleShowColumnTags
    | ToggleShowFilteredTags


boardNameDragType : String
boardNameDragType =
    "card-board-settings-board-name"


columnSettingsDragType : String
columnSettingsDragType =
    "card-board-settings-column-settings"


switchSettingsState : (SettingsState -> SettingsState) -> Model -> Model
switchSettingsState fn model =
    let
        currentSelectedFilters : List Filter
        currentSelectedFilters =
            Dict.values <| MultiSelect.selectedItems model.multiSelect

        newSettingsState : SettingsState
        newSettingsState =
            model.settingsState
                |> SettingsState.mapBoardBeingEdited (\bc -> { bc | filters = currentSelectedFilters })
                |> SettingsState.mapGlobalSettings (\gs -> { gs | filters = currentSelectedFilters })
                |> fn

        newFilters : Dict String Filter
        newFilters =
            case newSettingsState of
                SettingsState.EditingGlobalSettings _ ->
                    newSettingsState
                        |> SettingsState.settings
                        |> Settings.globalSettings
                        |> .filters
                        |> (\fs -> List.map (\f -> ( Filter.value f, f )) fs)
                        |> Dict.fromList

                _ ->
                    currentFilters <| SettingsState.boardConfigs newSettingsState

        newGrouper : List (MultiSelect.SelectionItem Filter) -> List ( String, List (MultiSelect.SelectionItem Filter) )
        newGrouper =
            case newSettingsState of
                SettingsState.EditingGlobalSettings _ ->
                    groupedSelectionsWithoutTags

                _ ->
                    groupedSelections
    in
    { model
        | settingsState = newSettingsState
        , multiSelect =
            MultiSelect.updateSelectedItems newFilters model.multiSelect
                |> MultiSelect.updateGrouper newGrouper
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
                    (SettingsState.addBoardConfirmed defaultColumnNames)
                    model

        AddColumnClicked ->
            mapSettingsState SettingsState.addColumnRequested model

        AddColumnConfirmed ->
            wrap <|
                switchSettingsState
                    SettingsState.addColumnConfirmed
                    model

        BackspacePressed ->
            wrap { model | multiSelect = MultiSelect.deleteHighlightedItem model.multiSelect }

        BoardNameClicked index ->
            wrap <| switchSettingsState (SettingsState.editBoardAt index) model

        BoardNameMouseDown ( domId, clientData ) ->
            ( { model | session = Session.waitForDrag clientData model.session }
            , InteropPorts.trackDraggable boardNameDragType clientData.clientPos domId
            , Session.NoOp
            )

        ColumnDeleteClicked index ->
            mapSettingsState (SettingsState.deleteColumnRequested index) model

        ColumnSettingsMouseDown ( domId, clientData ) ->
            ( { model | session = Session.waitForDrag clientData model.session }
            , InteropPorts.trackDraggable columnSettingsDragType clientData.clientPos domId
            , Session.NoOp
            )

        DeleteBoardRequested ->
            mapSettingsState SettingsState.deleteBoardRequested model

        DeleteConfirmed ->
            wrap <| switchSettingsState SettingsState.deleteConfirmed model

        ElementDragged dragData ->
            case dragData.dragAction of
                DragData.Move ->
                    if dragData.dragType == boardNameDragType then
                        ( model
                            |> updateBoardOrder (Session.dragTracker model.session) dragData
                            |> (\m -> { m | session = Session.moveDragable dragData m.session })
                        , Cmd.none
                        , Session.NoOp
                        )

                    else if dragData.dragType == columnSettingsDragType then
                        ( model
                            |> updateColumnOrder (Session.dragTracker model.session) dragData
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

        EnteredColumnCompletedLimit columnIndex limit ->
            mapCurrentColumnsForm (ColumnsForm.updateCompletedColumnLimit columnIndex limit) model

        EnteredColumnName columnIndex name ->
            mapCurrentColumnsForm (ColumnsForm.updateColumnName columnIndex name) model

        EnteredColumnNamedTagTag columnIndex tag ->
            mapCurrentColumnsForm (ColumnsForm.updateNamedTagTag columnIndex tag) model

        EnteredDatedColumnRangeValueFrom index value ->
            mapCurrentColumnsForm (ColumnsForm.updateDatedColumnRangeValueFrom index value) model

        EnteredDatedColumnRangeValueTo index value ->
            mapCurrentColumnsForm (ColumnsForm.updateDatedColumnRangeValueTo index value) model

        EnteredDefaultColumnName column name ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            (SettingsForm.updateDefaultColumnName column name)
                            model.settingsState
                }

        EnteredNewBoardName name ->
            mapBoardBeingAdded (NewBoardForm.updateName name) model

        EnteredNewColumnName name ->
            mapColumnBeingAdded (NewColumnForm.updateName name) model

        EnteredName name ->
            mapBoardBeingEdited (BoardConfigForm.updateName name) model

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
            wrap <| switchSettingsState SettingsState.editGlobalSettings model

        ModalCancelClicked ->
            handleClose model

        ModalCloseClicked ->
            handleClose model

        NewBoardTypeSelected boardType ->
            mapBoardBeingAdded (NewBoardForm.updateBoardType boardType) model

        NewColumnTypeSelected columnType ->
            mapColumnBeingAdded (NewColumnForm.updateColumnType columnType) model

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
            mapBoardBeingEdited (BoardConfigForm.updateFilterPolarity polarity) model

        ScopeSelected scope ->
            mapBoardBeingEdited (BoardConfigForm.updateFilterScope scope) model

        SelectedDatedColumnRangeType index rangeType ->
            mapCurrentColumnsForm (ColumnsForm.updateDatedColumnRangeType index rangeType) model

        TaskCompletionFormatSelected taskCompletionFormat ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            (SettingsForm.updateTaskCompletionFormat taskCompletionFormat)
                            model.settingsState
                }

        ToggleTaskCompletionInLocalTime ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            SettingsForm.toggleTaskCompletionInLocalTime
                            model.settingsState
                }

        ToggleTaskCompletionShowUtcOffset ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            SettingsForm.toggleTaskCompletionShowUtcOffset
                            model.settingsState
                }

        ToggleIgnoreFileNameDate ->
            wrap
                { model
                    | settingsState =
                        SettingsState.mapGlobalSettings
                            SettingsForm.toggleIgnoreFileNameDate
                            model.settingsState
                }

        ToggleShowColumnTags ->
            mapBoardBeingEdited BoardConfigForm.toggleShowColumnTags model

        ToggleShowFilteredTags ->
            mapBoardBeingEdited BoardConfigForm.toggleShowFilteredTags model


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


groupedSelectionsWithoutTags : List (MultiSelect.SelectionItem Filter) -> List ( String, List (MultiSelect.SelectionItem Filter) )
groupedSelectionsWithoutTags selectionItems =
    selectionItems
        |> List.sortBy (\item -> Filter.filterType item.value)
        |> LE.groupWhile (\a b -> Filter.filterType a.value == Filter.filterType b.value)
        |> List.map (\( i, is ) -> ( Filter.filterType i.value, i :: is ))
        |> ensureAllTypes
        |> List.filter (\( k, _ ) -> k /= "Tags")


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
                |> Settings.restrictSpecialColumns
                |> Settings.cleanupNames
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


mapBoardBeingAdded : (NewBoardForm -> NewBoardForm) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapBoardBeingAdded fn model =
    wrap { model | settingsState = SettingsState.mapBoardBeingAdded fn model.settingsState }


mapColumnBeingAdded : (NewColumnForm -> NewColumnForm) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapColumnBeingAdded fn model =
    wrap { model | settingsState = SettingsState.mapColumnBeingAdded fn model.settingsState }


mapBoardBeingEdited : (BoardConfigForm -> BoardConfigForm) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapBoardBeingEdited fn model =
    wrap { model | settingsState = SettingsState.mapBoardBeingEdited fn model.settingsState }


mapCurrentColumnsForm : (ColumnsForm -> ColumnsForm) -> Model -> ( Model, Cmd Msg, Session.Msg )
mapCurrentColumnsForm fn model =
    wrap { model | settingsState = SettingsState.mapCurrentColumnsForm fn model.settingsState }


updateBoardOrder : DragTracker -> DragData -> Model -> Model
updateBoardOrder dragTracker { cursor, beacons } model =
    case dragTracker of
        DragTracker.Dragging clientData _ ->
            case Rect.closestTo Coords.Vertical cursor beacons of
                Nothing ->
                    model

                Just position ->
                    { model
                        | settingsState =
                            SettingsState.moveBoard clientData.uniqueId position model.settingsState
                    }

        _ ->
            model


updateColumnOrder : DragTracker -> DragData -> Model -> Model
updateColumnOrder dragTracker { cursor, beacons } model =
    case dragTracker of
        DragTracker.Dragging clientData _ ->
            case Rect.closestTo Coords.Vertical cursor beacons of
                Nothing ->
                    model

                Just position ->
                    { model
                        | settingsState =
                            SettingsState.moveColumn clientData.uniqueId position model.settingsState
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
        SettingsState.AddingBoard newConfig settingsForm ->
            Html.div []
                [ boardSettingsView settingsForm model.multiSelect dragTracker
                , modalAddBoard newConfig
                ]

        SettingsState.AddingColumn newConfig settingsForm ->
            Html.div []
                [ boardSettingsView settingsForm model.multiSelect dragTracker
                , modalAddColumn newConfig settingsForm
                ]

        SettingsState.ClosingPlugin _ ->
            Html.text ""

        SettingsState.ClosingSettings _ ->
            Html.text ""

        SettingsState.DeletingBoard settingsForm ->
            Html.div []
                [ boardSettingsView settingsForm model.multiSelect dragTracker
                , modalConfirmDeleteBoard (settingsForm |> SettingsForm.boardConfigForms |> SafeZipper.current)
                ]

        SettingsState.DeletingColumn index settingsForm ->
            Html.div []
                [ boardSettingsView settingsForm model.multiSelect dragTracker
                , modalConfirmDeleteColumn
                    (settingsForm
                        |> SettingsForm.boardConfigForms
                        |> SafeZipper.current
                        |> Maybe.map .columnsForm
                        |> Maybe.map .columnForms
                        |> Maybe.map (LE.getAt index)
                        |> ME.join
                    )
                ]

        SettingsState.EditingBoard settingsForm ->
            boardSettingsView settingsForm model.multiSelect dragTracker

        SettingsState.EditingGlobalSettings settingsForm ->
            globalSettingsView
                (Session.dataviewTaskCompletion <| toSession model)
                (Session.timeWithZone <| toSession model)
                model.multiSelect
                settingsForm
                dragTracker


modalAddBoard : NewBoardForm -> Html Msg
modalAddBoard newBoardConfigForm =
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
                            , value <| newBoardConfigForm.name
                            , onInput EnteredNewBoardName
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "form-item" ]
                    [ Html.div [ class "form-item-name" ]
                        [ Html.text "Start type" ]
                    , Html.div [ class "form-item-control" ]
                        [ Html.select
                            [ class "dropdown"
                            , onInput NewBoardTypeSelected
                            ]
                            (NewBoardForm.optionsForSelect newBoardConfigForm
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
                ]
            , Html.div [ class "modal-button-container" ]
                [ Html.button
                    [ class "mod-cta"
                    , onClick <| AddBoardConfirmed
                    ]
                    [ Html.text "Add"
                    ]
                , Html.button
                    [ onClick <| ModalCancelClicked
                    ]
                    [ Html.text "Cancel"
                    ]
                ]
            ]
        ]


modalAddColumn : NewColumnForm -> SettingsForm -> Html Msg
modalAddColumn newColumnConfigForm settingsForm =
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
                            , value <| newColumnConfigForm.name
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
                            (NewColumnForm.optionsForSelect
                                (settingsForm
                                    |> SettingsForm.boardConfigForms
                                    |> SafeZipper.current
                                    |> Maybe.map .columnsForm
                                    |> Maybe.withDefault ColumnsForm.empty
                                )
                                newColumnConfigForm
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
                ]
            , Html.div [ class "modal-button-container" ]
                [ Html.button
                    [ class "mod-cta"
                    , onClick <| AddColumnConfirmed
                    ]
                    [ Html.text "Add"
                    ]
                , Html.button
                    [ onClick <| ModalCancelClicked
                    ]
                    [ Html.text "Cancel"
                    ]
                ]
            ]
        ]


modalConfirmDeleteBoard : Maybe BoardConfigForm -> Html Msg
modalConfirmDeleteBoard boardConfigForm =
    case boardConfigForm of
        Just configForm ->
            let
                nameString : String
                nameString =
                    if String.length configForm.name == 0 then
                        "this un-named board"

                    else
                        "the board named " ++ configForm.name
            in
            Html.div [ class "modal-container" ]
                [ Html.div
                    [ class "modal-bg"
                    , style "opacity" "0.85"
                    ]
                    []
                , Html.div [ class "modal" ]
                    [ Html.div
                        [ class "modal-close-button"
                        , onClick ModalCloseClicked
                        ]
                        []
                    , Html.div [ class "modal-title" ]
                        [ Html.text "Delete Board" ]
                    , Html.div [ class "modal-content" ]
                        [ Html.p [ class "mod-warning" ]
                            [ Html.text <|
                                "This will delete "
                                    ++ nameString
                                    ++ ".  It will not affect any other boards or any files in your vault."
                            ]
                        ]
                    , Html.div [ class "modal-button-container" ]
                        [ Html.button
                            [ class "mod-warning"
                            , onClick <| DeleteConfirmed
                            ]
                            [ Html.text "Delete"
                            ]
                        , Html.button
                            [ onClick <| ModalCancelClicked
                            ]
                            [ Html.text "Cancel"
                            ]
                        ]
                    ]
                ]

        Nothing ->
            Html.text ""


modalConfirmDeleteColumn : Maybe ColumnForm -> Html Msg
modalConfirmDeleteColumn columnForm =
    case columnForm of
        Just form ->
            let
                formName : String
                formName =
                    ColumnForm.name form

                nameString : String
                nameString =
                    if String.length formName == 0 then
                        "this un-named " ++ typeString ++ " column"

                    else
                        "the " ++ typeString ++ " column named " ++ formName

                typeString : String
                typeString =
                    ColumnForm.typeString form
            in
            Html.div [ class "modal-container" ]
                [ Html.div
                    [ class "modal-bg"
                    , style "opacity" "0.85"
                    ]
                    []
                , Html.div [ class "modal" ]
                    [ Html.div
                        [ class "modal-close-button"
                        , onClick ModalCloseClicked
                        ]
                        []
                    , Html.div [ class "modal-title" ]
                        [ Html.text "Delete Column" ]
                    , Html.div [ class "modal-content" ]
                        [ Html.p [ class "mod-warning" ]
                            [ Html.text <|
                                "This will delete "
                                    ++ nameString
                                    ++ ".  It will not affect any other boards, columns, or any files in your vault."
                            ]
                        ]
                    , Html.div [ class "modal-button-container" ]
                        [ Html.button
                            [ class "mod-warning"
                            , onClick <| DeleteConfirmed
                            ]
                            [ Html.text "Delete"
                            ]
                        , Html.button
                            [ onClick <| ModalCancelClicked
                            ]
                            [ Html.text "Cancel"
                            ]
                        ]
                    ]
                ]

        Nothing ->
            Html.text ""


settingsSurroundView : CurrentSection -> SafeZipper BoardConfigForm -> DragTracker -> List (Html Msg) -> Html Msg
settingsSurroundView currentSection boardConfigForms dragTracker formContents =
    let
        boardMapFn : Int -> BoardConfigForm -> Html Msg
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
            DragTracker.isDragging dragTracker && draggedType == Just boardNameDragType

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
                [ Html.div [ class "settings-menu vertical-tab-header card-board-settings-board-name-container" ]
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
                            (boardConfigForms
                                |> SafeZipper.indexedMapSelectedAndRest boardMapFn settingNameView
                                |> SafeZipper.toList
                                |> (\hs ->
                                        List.append hs
                                            [ settingNameDraggedView isDragging (SafeZipper.current boardConfigForms) dragTracker ]
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


boardSettingsView : SettingsForm -> MultiSelect.Model Msg Filter -> DragTracker -> Html Msg
boardSettingsView settingsForm multiSelect dragTracker =
    boardSettingsForm
        (SafeZipper.current <| SettingsForm.boardConfigForms settingsForm)
        (SafeZipper.currentIndex <| SettingsForm.boardConfigForms settingsForm)
        (SettingsForm.defaultColumnNames settingsForm)
        multiSelect
        dragTracker
        |> settingsSurroundView Boards settingsForm.boardConfigForms dragTracker


globalSettingsView : DataviewTaskCompletion -> TimeWithZone -> MultiSelect.Model Msg Filter -> SettingsForm -> DragTracker -> Html Msg
globalSettingsView dataviewTaskCompletion timeWithZone multiSelect settingsForm dragTracker =
    settingsForm
        |> globalSettingsForm dataviewTaskCompletion timeWithZone multiSelect
        |> settingsSurroundView Options settingsForm.boardConfigForms dragTracker


globalSettingsForm : DataviewTaskCompletion -> TimeWithZone -> MultiSelect.Model Msg Filter -> SettingsForm -> List (Html Msg)
globalSettingsForm dataviewTaskCompletion timeWithZone multiSelect settingsForm =
    let
        ignoreFileNameDatesStyle : String
        ignoreFileNameDatesStyle =
            if settingsForm.ignoreFileNameDates then
                " is-enabled"

            else
                ""

        taskCompletionInLocalTimeStyle : String
        taskCompletionInLocalTimeStyle =
            if settingsForm.taskCompletionInLocalTime then
                " is-enabled"

            else
                ""

        taskCompletionShowUtcOffsetStyle : String
        taskCompletionShowUtcOffsetStyle =
            if settingsForm.taskCompletionShowUtcOffset then
                " is-enabled"

            else
                ""

        taskCompletionExample : String
        taskCompletionExample =
            let
                taskCompletionSettings : TaskCompletionSettings
                taskCompletionSettings =
                    settingsForm
                        |> SD.run SettingsForm.safeDecoder
                        |> Result.map Settings.globalSettings
                        |> Result.withDefault GlobalSettings.default
                        |> GlobalSettings.taskCompletionSettings
            in
            TimeWithZone.completionString
                dataviewTaskCompletion
                taskCompletionSettings
                timeWithZone
                |> (\str ->
                        if String.length str == 0 then
                            "[None]"

                        else
                            str
                   )
    in
    [ Html.div [ class "setting-items-inner" ]
        ([ Html.div [ class "setting-item setting-item-heading" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Daily/Periodic Notes Compatibility" ]
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
                    [ Html.text "File/Path Filters" ]
                , Html.div [ class "setting-item-description" ] []
                ]
            , Html.div [ class "setting-item-control" ] []
            ]
         , Html.div [ class "setting-item" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Files and paths to ignore" ]
                , Html.div [ class "setting-item-description" ]
                    [ Html.text "Tasks will not be loaded from these.  "
                    , Html.br [] []
                    , Html.br [] []
                    , Html.span
                        [ class "mod-warning" ]
                        [ Html.text "You need to reload Obsidian to see the effect of any changes made to this setting." ]
                    ]
                ]
            , Html.div [ class "setting-item-control" ]
                [ MultiSelect.view multiSelect
                ]
            ]
         , Html.div [ class "setting-item setting-item-heading" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Task Completion" ]
                , Html.div [ class "setting-item-description" ]
                    [ Html.text <| "Current completion string: " ++ taskCompletionExample ]
                ]
            , Html.div [ class "setting-item-control" ] []
            ]
         , Html.div [ class "setting-item" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Format" ]
                , Html.div [ class "setting-item-description" ] []
                ]
            , Html.div [ class "setting-item-control" ]
                [ taskCompletionFormatSelect settingsForm.taskCompletionFormat ]
            ]
         , Html.div [ class "setting-item" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Use local time" ]
                , Html.div [ class "setting-item-description" ]
                    [ Html.text "UTC will be used otherwise." ]
                ]
            , Html.div [ class "setting-item-control" ]
                [ Html.div
                    [ class <| "checkbox-container" ++ taskCompletionInLocalTimeStyle
                    , onClick ToggleTaskCompletionInLocalTime
                    ]
                    []
                ]
            ]
         , Html.div [ class "setting-item" ]
            [ Html.div [ class "setting-item-info" ]
                [ Html.div [ class "setting-item-name" ]
                    [ Html.text "Include UTC offset" ]
                , Html.div [ class "setting-item-description" ] []
                ]
            , Html.div [ class "setting-item-control" ]
                [ Html.div
                    [ class <| "checkbox-container" ++ taskCompletionShowUtcOffsetStyle
                    , onClick ToggleTaskCompletionShowUtcOffset
                    ]
                    []
                ]
            ]
         ]
            ++ columNamesForm settingsForm
        )
    ]


columNamesForm : SettingsForm -> List (Html Msg)
columNamesForm settingsForm =
    [ Html.div [ class "setting-item setting-item-heading" ]
        [ Html.div [ class "setting-item-info" ]
            [ Html.div [ class "setting-item-name" ]
                [ Html.text "Default Column Names" ]
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
                , value settingsForm.today
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
                , value settingsForm.tomorrow
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
                , value settingsForm.future
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
                , value settingsForm.undated
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
                , value settingsForm.otherTags
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
                , value settingsForm.untagged
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
                , value settingsForm.completed
                , onInput (EnteredDefaultColumnName "completed")
                ]
                []
            ]
        ]
    ]


boardSettingsForm : Maybe BoardConfigForm -> Maybe Int -> DefaultColumnNames -> MultiSelect.Model Msg Filter -> DragTracker -> List (Html Msg)
boardSettingsForm boardConfigForm boardIndex defaultColumnNames multiSelect dragTracker =
    case ( boardConfigForm, boardIndex ) of
        ( Just configForm, Just _ ) ->
            let
                draggedColumn : Maybe ColumnForm
                draggedColumn =
                    ColumnsForm.find (\f -> Just (ColumnForm.name f) == draggedUniqueId) configForm.columnsForm

                draggedType : Maybe String
                draggedType =
                    DragTracker.dragType dragTracker

                draggedUniqueId : Maybe String
                draggedUniqueId =
                    if isDragging then
                        DragTracker.uniqueId dragTracker

                    else
                        Nothing

                isDragging : Bool
                isDragging =
                    DragTracker.isDragging dragTracker && draggedType == Just columnSettingsDragType

                showFilteredTagsStyle : String
                showFilteredTagsStyle =
                    if configForm.showFilteredTags then
                        " is-enabled"

                    else
                        ""

                showColumnTagsStyle : String
                showColumnTagsStyle =
                    if configForm.showColumnTags then
                        " is-enabled"

                    else
                        ""
            in
            [ Html.div
                [ class "setting-item"
                , attributeIf isDragging (style "cursor" "grabbing")
                ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Name" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "The name of this board" ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ Html.input
                        [ type_ "text"
                        , value <| configForm.name
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
                    [ MultiSelect.view multiSelect
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
                    [ polaritySelect <| configForm.filterPolarity ]
                ]
            , Html.div [ class "setting-item" ]
                [ Html.div [ class "setting-item-info" ]
                    [ Html.div [ class "setting-item-name" ]
                        [ Html.text "Tag filter scope" ]
                    , Html.div [ class "setting-item-description" ]
                        [ Html.text "Apply tag filters to just the top level tasks, to just sub-tasks, or to both." ]
                    ]
                , Html.div [ class "setting-item-control" ]
                    [ scopeSelect <| configForm.filterScope ]
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
                    ((configForm.columnsForm
                        |> .columnForms
                        |> List.indexedMap (settingsColumnView draggedUniqueId defaultColumnNames)
                     )
                        |> (\hs ->
                                List.append hs
                                    [ settingsColumnDraggedView isDragging draggedColumn dragTracker ]
                           )
                    )
                ]
            , Html.div [ class "cardboard-settings-columns-button" ]
                [ Html.div [ class "setting-item-control" ]
                    [ Html.button
                        [ onClick AddColumnClicked ]
                        [ Html.text "Add Column" ]
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


settingsColumnView : Maybe String -> DefaultColumnNames -> Int -> ColumnForm -> Html Msg
settingsColumnView uniqueId defaultColumnNames index columnForm =
    let
        domId : String
        domId =
            "card-board-setting-column-settings:" ++ String.fromInt index

        isBeingDragged : Bool
        isBeingDragged =
            uniqueId == Just name

        name : String
        name =
            ColumnForm.name columnForm

        defaultName : String
        defaultName =
            ColumnForm.placeholder defaultColumnNames columnForm
    in
    Html.div []
        [ columnSettingsBeacon (BeaconPosition.Before name)
        , Html.div
            [ id domId
            , class "cardboard-settings-column-item"
            , attributeIf isBeingDragged (style "opacity" "0.0")
            ]
            [ Html.div
                [ class "cardboard-settings-column-item-detail card-board-dragable"
                , onDown
                    (\e ->
                        ColumnSettingsMouseDown <|
                            ( domId
                            , { uniqueId = name
                              , clientPos = Coords.fromFloatTuple e.clientPos
                              , offsetPos = Coords.fromFloatTuple e.offsetPos
                              }
                            )
                    )
                ]
                [ FeatherIcons.toHtml
                    []
                    dragIcon
                ]
            , Html.div [ class "cardboard-settings-column-item-type" ]
                [ Html.text <| ColumnForm.typeString columnForm ]
            , Html.div [ class "cardboard-settings-column-item-detail" ]
                [ Html.input
                    [ type_ "text"
                    , placeholder defaultName
                    , value name
                    , onInput <| EnteredColumnName index
                    ]
                    []
                ]
            , settingsColumnControlView index columnForm
            , Html.div
                [ class "cardboard-settings-column-item-button"
                , onClick <| ColumnDeleteClicked index
                ]
                [ FeatherIcons.xCircle
                    |> FeatherIcons.withSize 1
                    |> FeatherIcons.withSizeUnit "em"
                    |> FeatherIcons.toHtml []
                ]
            ]
        , columnSettingsBeacon (BeaconPosition.After name)
        ]


settingsColumnDraggedView : Bool -> Maybe ColumnForm -> DragTracker -> Html Msg
settingsColumnDraggedView isDragging columnForm dragTracker =
    case ( isDragging, columnForm, dragTracker ) of
        ( True, Just draggedColumnForm, DragTracker.Dragging clientData domData ) ->
            Html.div []
                [ Html.div
                    [ class "cardboard-settings-column-item"
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
                    [ FeatherIcons.toHtml [] dragIcon
                    , Html.div [ class "cardboard-settings-column-item-type" ]
                        [ Html.text <| ColumnForm.typeString draggedColumnForm ]
                    , Html.div [ class "cardboard-settings-column-item-detail" ]
                        [ Html.input
                            [ type_ "text"
                            , value <| ColumnForm.name draggedColumnForm
                            ]
                            []
                        ]
                    , settingsColumnControlView 0 draggedColumnForm
                    , Html.div
                        [ class "cardboard-settings-column-item-button" ]
                        [ FeatherIcons.xCircle
                            |> FeatherIcons.withSize 1
                            |> FeatherIcons.withSizeUnit "em"
                            |> FeatherIcons.toHtml []
                        ]
                    ]
                ]

        _ ->
            Html.text ""


settingsColumnControlView : Int -> ColumnForm -> Html Msg
settingsColumnControlView index columnForm =
    case columnForm of
        ColumnForm.CompletedColumnForm _ completedForm ->
            Html.div [ class "cardboard-settings-column-item-controls" ]
                [ Html.text <| "Limit: "
                , Html.input
                    [ type_ "text"
                    , placeholder "10"
                    , value completedForm.limit
                    , attribute "size" "3"
                    , onInput <| EnteredColumnCompletedLimit index
                    ]
                    []
                ]

        ColumnForm.DatedColumnForm _ datedForm ->
            Html.div [ class "cardboard-settings-column-item-controls" ]
                ([ rangeSelectView index datedForm ]
                    ++ rangeInputsView index datedForm
                )

        ColumnForm.NamedTagColumnForm _ namedTagForm ->
            Html.div [ class "cardboard-settings-column-item-controls" ]
                [ Html.text <| "Tag: "
                , Html.input
                    [ type_ "text"
                    , value <| namedTagForm.tag
                    , onInput <| EnteredColumnNamedTagTag index
                    ]
                    []
                ]

        _ ->
            Html.div [ class "cardboard-settings-column-item-controls" ]
                [ Html.text "" ]


rangeSelectView : Int -> DatedColumnForm -> Html Msg
rangeSelectView index datedColumnForm =
    case datedColumnForm.rangeType of
        "After" ->
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

        "Between" ->
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

        _ ->
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


rangeInputsView : Int -> DatedColumnForm -> List (Html Msg)
rangeInputsView index datedColumnForm =
    case datedColumnForm.rangeType of
        "After" ->
            [ Html.input
                [ type_ "text"
                , placeholder "0"
                , value datedColumnForm.from
                , attribute "size" "3"
                , onInput <| EnteredDatedColumnRangeValueFrom index
                ]
                []
            ]

        "Between" ->
            [ Html.input
                [ type_ "text"
                , placeholder "0"
                , value datedColumnForm.from
                , attribute "size" "3"
                , onInput <| EnteredDatedColumnRangeValueFrom index
                ]
                []
            , Html.span [] [ Html.text "and" ]
            , Html.input
                [ type_ "text"
                , placeholder "0"
                , value datedColumnForm.to
                , attribute "size" "3"
                , onInput <| EnteredDatedColumnRangeValueTo index
                ]
                []
            ]

        _ ->
            [ Html.input
                [ type_ "text"
                , placeholder "0"
                , value datedColumnForm.to
                , attribute "size" "3"
                , onInput <| EnteredDatedColumnRangeValueTo index
                ]
                []
            ]


taskCompletionFormatSelect : String -> Html Msg
taskCompletionFormatSelect taskCompletionFormat =
    Html.select
        [ class "dropdown"
        , onInput TaskCompletionFormatSelected
        ]
        (case taskCompletionFormat of
            "ObsidianCardBoard" ->
                [ Html.option [ value "NoCompletion" ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard", selected True ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview" ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks" ]
                    [ Html.text "Tasks" ]
                ]

            "ObsidianDataview" ->
                [ Html.option [ value "NoCompletion" ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard" ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview", selected True ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks" ]
                    [ Html.text "Tasks" ]
                ]

            "ObsidianTasks" ->
                [ Html.option [ value "NoCompletion" ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard" ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview" ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks", selected True ]
                    [ Html.text "Tasks" ]
                ]

            _ ->
                [ Html.option [ value "NoCompletion", selected True ]
                    [ Html.text "None" ]
                , Html.option [ value "ObsidianCardBoard" ]
                    [ Html.text "CardBoard" ]
                , Html.option [ value "ObsidianDataview" ]
                    [ Html.text "Dataview" ]
                , Html.option [ value "ObsidianTasks" ]
                    [ Html.text "Tasks" ]
                ]
        )


scopeSelect : String -> Html Msg
scopeSelect scope =
    Html.select
        [ class "dropdown"
        , onInput ScopeSelected
        ]
        (case scope of
            "TopLevelOnly" ->
                [ Html.option [ value "TopLevelOnly", selected True ]
                    [ Html.text "Top Level" ]
                , Html.option [ value "SubTasksOnly" ]
                    [ Html.text "Sub-tasks" ]
                , Html.option [ value "Both" ]
                    [ Html.text "Both" ]
                ]

            "SubTasksOnly" ->
                [ Html.option [ value "TopLevelOnly" ]
                    [ Html.text "Top Level" ]
                , Html.option [ value "SubTasksOnly", selected True ]
                    [ Html.text "Sub-tasks" ]
                , Html.option [ value "Both" ]
                    [ Html.text "Both" ]
                ]

            _ ->
                [ Html.option [ value "TopLevelOnly" ]
                    [ Html.text "Top Level" ]
                , Html.option [ value "SubTasksOnly" ]
                    [ Html.text "Sub-tasks" ]
                , Html.option [ value "Both", selected True ]
                    [ Html.text "Both" ]
                ]
        )


polaritySelect : String -> Html Msg
polaritySelect polarity =
    Html.select
        [ class "dropdown"
        , onInput PolaritySelected
        ]
        (case polarity of
            "Allow" ->
                [ Html.option [ value "Allow", selected True ]
                    [ Html.text "Allow" ]
                , Html.option [ value "Deny" ]
                    [ Html.text "Deny" ]
                ]

            _ ->
                [ Html.option [ value "Allow" ]
                    [ Html.text "Allow" ]
                , Html.option [ value "Deny", selected True ]
                    [ Html.text "Deny" ]
                ]
        )


settingNameView : Int -> BoardConfigForm -> Html Msg
settingNameView index boardConfigForm =
    let
        domId : String
        domId =
            "card-board-setting-board-name:" ++ String.fromInt index

        name : String
        name =
            boardConfigForm.name
    in
    Html.div []
        [ boardNameBeacon (BeaconPosition.Before name)
        , Html.div
            [ id domId
            , class "vertical-tab-nav-item card-board-dragable"
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
        , boardNameBeacon (BeaconPosition.After name)
        ]


settingNameSelectedView : Bool -> Int -> BoardConfigForm -> Html Msg
settingNameSelectedView isDragging index boardConfigForm =
    let
        domId : String
        domId =
            "card-board-setting-board-name:" ++ String.fromInt index

        name : String
        name =
            boardConfigForm.name
    in
    Html.div []
        [ boardNameBeacon (BeaconPosition.Before name)
        , Html.div
            [ id domId
            , class "vertical-tab-nav-item card-board-dragable is-active"
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
        , boardNameBeacon (BeaconPosition.After name)
        ]


settingNameDraggedView : Bool -> Maybe BoardConfigForm -> DragTracker -> Html Msg
settingNameDraggedView isDragging boardConfigForm dragTracker =
    case ( isDragging, boardConfigForm, dragTracker ) of
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
                    [ Html.text config.name ]
                ]

        _ ->
            Html.text ""



-- HELPERS


attributeIf : Bool -> Attribute msg -> Attribute msg
attributeIf condition attribute =
    if condition then
        attribute

    else
        class ""


boardNameBeaconType : String
boardNameBeaconType =
    "data-" ++ boardNameDragType ++ "-beacon"


boardNameBeacon : BeaconPosition -> Html Msg
boardNameBeacon beaconPosition =
    Html.span
        [ attribute boardNameBeaconType (JE.encode 0 <| BeaconPosition.encoder beaconPosition)
        , style "font-size" "0"
        ]
        []


columnSettingsBeaconType : String
columnSettingsBeaconType =
    "data-" ++ columnSettingsDragType ++ "-beacon"


columnSettingsBeacon : BeaconPosition -> Html Msg
columnSettingsBeacon beaconPosition =
    Html.span
        [ attribute columnSettingsBeaconType (JE.encode 0 <| BeaconPosition.encoder beaconPosition)
        , style "font-size" "0"
        ]
        []


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
        |> FeatherIcons.withSize 22
        |> FeatherIcons.withViewBox "0 0 32 32"
