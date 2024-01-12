module Page.Board exposing
    ( Model
    , Msg(..)
    , init
    , mapSession
    , toSession
    , update
    , view
    )

import Board exposing (Board)
import Boards exposing (Boards)
import Card exposing (Card)
import Column exposing (Column)
import Columns
import Date exposing (Date)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords
import DragAndDrop.DragData as DragData exposing (DragData)
import DragAndDrop.DragTracker as DragTracker exposing (DragTracker)
import DragAndDrop.Rect as Rect
import FeatherIcons
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, checked, class, hidden, id, style, type_)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse exposing (onDown)
import Html.Keyed
import Html.Lazy
import InteropPorts
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE
import Maybe.Extra as ME
import SafeZipper
import Session exposing (Session)
import TagList
import TaskItem exposing (TaskItem, TaskItemFields)
import TextDirection
import TimeWithZone exposing (TimeWithZone)



-- MODEL


type Model
    = ViewingBoard Session
    | DeletingCard String String Session


init : Session -> Model
init session =
    ViewingBoard session


cancelCurrentState : Model -> Model
cancelCurrentState model =
    ViewingBoard (toSession model)


deleteCardRequested : String -> String -> Model -> Model
deleteCardRequested title cardId model =
    DeletingCard title cardId (toSession model)


deleteCardConfirmed : Model -> Model
deleteCardConfirmed model =
    ViewingBoard (toSession model)


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    case model of
        ViewingBoard session ->
            ViewingBoard <| fn session

        DeletingCard title cardId session ->
            DeletingCard title cardId <| fn session


toSession : Model -> Session
toSession model =
    case model of
        ViewingBoard session ->
            session

        DeletingCard _ _ session ->
            session



-- UPDATE


type Msg
    = ColumnMouseDown ( String, DragTracker.ClientData )
    | DeleteConfirmed String
    | ElementDragged DragData
    | ModalCancelClicked
    | ModalCloseClicked
    | SettingsClicked
    | TabHeaderMouseDown ( String, DragTracker.ClientData )
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String String
    | TaskItemToggled String
    | ToggleColumnCollapse Int Bool


tagHeaderDragType : String
tagHeaderDragType =
    "card-board-tag-header"


columnDragType : String
columnDragType =
    "card-board-column"


update : Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update msg model =
    case msg of
        ColumnMouseDown ( domId, clientData ) ->
            ( mapSession (Session.waitForDrag clientData) model
            , InteropPorts.trackDraggable columnDragType clientData.clientPos domId
            , Session.NoOp
            )

        DeleteConfirmed cardId ->
            ( deleteCardConfirmed model
            , cmdIfHasTask cardId model InteropPorts.deleteTask
            , Session.NoOp
            )

        ElementDragged dragData ->
            case dragData.dragAction of
                DragData.Move ->
                    if dragData.dragType == tagHeaderDragType then
                        ( model
                            |> updateBoardOrder (Session.dragTracker <| toSession model) dragData
                            |> (mapSession <| Session.moveDragable dragData)
                        , Cmd.none
                        , Session.NoOp
                        )

                    else if dragData.dragType == columnDragType then
                        ( model
                            |> updateColumnOrder (Session.dragTracker <| toSession model) dragData
                            |> (mapSession <| Session.moveDragable dragData)
                        , Cmd.none
                        , Session.NoOp
                        )

                    else
                        ( model, Cmd.none, Session.NoOp )

                DragData.Stop ->
                    ( mapSession Session.stopTrackingDragable model
                    , InteropPorts.updateSettings <| Session.settings <| toSession model
                    , Session.NoOp
                    )

        ModalCancelClicked ->
            ( cancelCurrentState model
            , Cmd.none
            , Session.NoOp
            )

        ModalCloseClicked ->
            ( cancelCurrentState model
            , Cmd.none
            , Session.NoOp
            )

        SettingsClicked ->
            ( model
            , Cmd.none
            , Session.SettingsClicked
            )

        TabHeaderMouseDown ( domId, clientData ) ->
            ( mapSession (Session.waitForDrag clientData) model
            , InteropPorts.trackDraggable tagHeaderDragType clientData.clientPos domId
            , Session.NoOp
            )

        TabSelected tabIndex ->
            ( mapSession (Session.switchToBoardAt tabIndex) model
            , Cmd.none
            , Session.NoOp
            )

        TaskItemDeleteClicked title id ->
            ( deleteCardRequested title id model
            , Cmd.none
            , Session.NoOp
            )

        TaskItemEditClicked id ->
            ( model
            , cmdIfHasTask id model InteropPorts.openTaskSourceFile
            , Session.NoOp
            )

        TaskItemToggled id ->
            let
                timeWithZone : TimeWithZone
                timeWithZone =
                    Session.timeWithZone (toSession model)

                toggleCmd : TaskItem -> Cmd Msg
                toggleCmd taskItem =
                    InteropPorts.rewriteTasks
                        (Session.dataviewTaskCompletion <| toSession model)
                        (Session.taskCompletionSettings <| toSession model)
                        timeWithZone
                        (TaskItem.filePath taskItem)
                        (TaskItem.tasksToToggle id timeWithZone taskItem)

                cmd : Cmd Msg
                cmd =
                    model
                        |> toSession
                        |> Session.taskContainingId id
                        |> Maybe.map toggleCmd
                        |> Maybe.withDefault Cmd.none
            in
            ( model
            , cmd
            , Session.NoOp
            )

        ToggleColumnCollapse columnIndex newState ->
            let
                newSession : Session
                newSession =
                    Session.updateColumnCollapse columnIndex newState (toSession model)
            in
            ( mapSession (always newSession) model
            , InteropPorts.updateSettings <| Session.settings newSession
            , Session.NoOp
            )


cmdIfHasTask : String -> Model -> (TaskItemFields -> Cmd b) -> Cmd b
cmdIfHasTask id model cmd =
    model
        |> toSession
        |> Session.taskFromId id
        |> Maybe.map TaskItem.fields
        |> Maybe.map cmd
        |> Maybe.withDefault Cmd.none


updateBoardOrder : DragTracker -> DragData -> Model -> Model
updateBoardOrder dragTracker { cursor, beacons } model =
    case dragTracker of
        DragTracker.Dragging clientData _ ->
            case Rect.closestTo cursor beacons of
                Nothing ->
                    model

                Just position ->
                    mapSession (Session.moveBoard clientData.uniqueId position) model

        _ ->
            model


updateColumnOrder : DragTracker -> DragData -> Model -> Model
updateColumnOrder dragTracker { cursor, beacons } model =
    case dragTracker of
        DragTracker.Dragging clientData _ ->
            case Rect.closestTo cursor beacons of
                Nothing ->
                    model

                Just position ->
                    mapSession (Session.moveColumn clientData.uniqueId position) model

        _ ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        ViewingBoard session ->
            Html.div []
                [ boardsView session ]

        DeletingCard title cardId session ->
            Html.div []
                [ boardsView session
                , modalDeleteCardConfirm title cardId
                ]


modalDeleteCardConfirm : String -> String -> Html Msg
modalDeleteCardConfirm title cardId =
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
                [ Html.text "Delete Card" ]
            , Html.div [ class "modal-content" ]
                [ Html.p [ class "mod-warning" ]
                    [ Html.text <|
                        "Press Delete to confirm you wish to delete the \""
                            ++ title
                            ++ "\" card.  This does not delete the task from your "
                            ++ "markdown, it encloses it in <del> tags."
                    ]
                ]
            , Html.div [ class "modal-button-container" ]
                [ Html.button
                    [ class "mod-warning"
                    , onClick <| DeleteConfirmed cardId
                    ]
                    [ Html.text "Delete"
                    ]
                , Html.button
                    [ onClick <| ModalCancelClicked ]
                    [ Html.text "Cancel"
                    ]
                ]
            ]
        ]


boardsView : Session -> Html Msg
boardsView session =
    if SafeZipper.length (Session.boardConfigs session) == 0 then
        Html.text "Loading tasks...."

    else
        let
            boards : Boards
            boards =
                Boards.init (Session.uniqueId session) (Session.boardConfigs session) (Session.taskList session)

            currentBoardIndex : Maybe Int
            currentBoardIndex =
                Boards.currentIndex boards

            ignoreFileNameDates : Bool
            ignoreFileNameDates =
                Session.ignoreFileNameDates session

            today : Date
            today =
                Session.timeWithZone session
                    |> TimeWithZone.toDate

            isDragging : Bool
            isDragging =
                Session.isDragging session && draggedType == Just tagHeaderDragType

            dragTracker : DragTracker
            dragTracker =
                session
                    |> Session.dragTracker

            draggedType : Maybe String
            draggedType =
                DragTracker.dragType dragTracker
        in
        Html.div
            [ attribute "dir" (TextDirection.toString <| Session.textDirection session)
            , attributeIf isDragging (style "cursor" "grabbing")
            ]
            [ Html.div [ class "workspace-tab-header-container" ]
                [ Html.div
                    [ class "sidebar-toggle-button"
                    , class "mod-left"
                    , attribute "aria-label" "Settings"
                    , attribute "aria-label-position" "right"
                    ]
                    [ Html.div
                        [ class "clickable-icon"
                        , onClick SettingsClicked
                        ]
                        [ FeatherIcons.settings
                            |> FeatherIcons.withSize 1
                            |> FeatherIcons.withSizeUnit "em"
                            |> FeatherIcons.toHtml []
                        ]
                    ]
                , Html.div
                    [ class "workspace-tab-header-container-inner" ]
                    (tabHeaders isDragging currentBoardIndex boards
                        ++ [ viewDraggedHeader isDragging session ]
                    )
                , Html.div
                    [ class "card-board-tab-header-spacer" ]
                    []
                ]
            , Html.Keyed.node "div"
                [ class "card-board-boards" ]
                (Boards.boardZipper boards
                    |> SafeZipper.mapSelectedAndRest
                        (keyedSelectedBoardView ignoreFileNameDates today dragTracker)
                        (keyedBoardView ignoreFileNameDates today)
                    |> SafeZipper.toList
                )
            ]


tabHeaders : Bool -> Maybe Int -> Boards -> List (Html Msg)
tabHeaders isDragging currentBoardIndex boards =
    Boards.names boards
        |> SafeZipper.indexedMapSelectedAndRest (selectedTabHeader isDragging) (tabHeader isDragging currentBoardIndex)
        |> SafeZipper.toList


tabHeader : Bool -> Maybe Int -> Int -> String -> Html Msg
tabHeader isDragging currentBoardIndex tabIndex name =
    let
        headerClass : String
        headerClass =
            tabHeaderClass currentBoardIndex tabIndex

        domId : String
        domId =
            "card-board-tab:" ++ String.fromInt tabIndex
    in
    Html.div
        [ id domId
        , class ("workspace-tab-header" ++ headerClass)
        , attributeIf (not isDragging) (attribute "aria-label" name)
        , attributeIf (not isDragging) (attribute "aria-label-delay" "50")
        , onClick <| TabSelected tabIndex
        , onDown
            (\e ->
                TabHeaderMouseDown <|
                    ( domId
                    , { uniqueId = name
                      , clientPos = Coords.fromFloatTuple e.clientPos
                      , offsetPos = Coords.fromFloatTuple e.offsetPos
                      }
                    )
            )
        ]
        [ beacon (BeaconPosition.Before name)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| name ]
            ]
        , beacon (BeaconPosition.After name)
        ]


selectedTabHeader : Bool -> Int -> String -> Html Msg
selectedTabHeader isDragging tabIndex name =
    let
        domId : String
        domId =
            "card-board-tab:" ++ String.fromInt tabIndex
    in
    Html.div
        [ class "workspace-tab-header is-active"
        , id domId
        , attributeIf (not isDragging) (attribute "aria-label" name)
        , attributeIf isDragging (attribute "aria-label" "")
        , attributeIf (not isDragging) (attribute "aria-label-delay" "50")
        , attributeIf isDragging (style "opacity" "0.0")
        , onDown
            (\e ->
                TabHeaderMouseDown <|
                    ( domId
                    , { uniqueId = name
                      , clientPos = Coords.fromFloatTuple e.clientPos
                      , offsetPos = Coords.fromFloatTuple e.offsetPos
                      }
                    )
            )
        ]
        [ beacon (BeaconPosition.Before name)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| name ]
            ]
        , beacon (BeaconPosition.After name)
        ]


viewDraggedHeader : Bool -> Session -> Html Msg
viewDraggedHeader isDragging session =
    case ( isDragging, Session.dragTracker session ) of
        ( True, DragTracker.Dragging clientData domData ) ->
            Html.div
                [ class "workspace-tab-header is-active"
                , id <| "card-board-tab:being-dragged"
                , style "position" "fixed"
                , style "top" (String.fromFloat (domData.draggedNodeStartRect.y - domData.offset.y) ++ "px")
                , style "left" (String.fromFloat (clientData.clientPos.x - domData.offset.x - clientData.offsetPos.x) ++ "px")
                , style "width" (String.fromFloat domData.draggedNodeStartRect.width ++ "px")
                , style "height" (String.fromFloat domData.draggedNodeStartRect.height ++ "px")
                , style "cursor" "grabbing"
                , style "opacity" "0.85"
                ]
                [ Html.div
                    [ class "workspace-tab-header-inner" ]
                    [ Html.div [ class "workspace-tab-header-inner-title" ]
                        [ Html.text <| clientData.uniqueId ]
                    ]
                ]

        _ ->
            empty


beaconType : String
beaconType =
    "data-" ++ tagHeaderDragType ++ "-beacon"


beacon : BeaconPosition -> Html Msg
beacon beaconPosition =
    Html.span
        [ attribute beaconType (JE.encode 0 <| BeaconPosition.encoder beaconPosition)
        , style "font-size" "0"
        ]
        []


tabHeaderClass : Maybe Int -> Int -> String
tabHeaderClass currentBoardIndex index =
    case currentBoardIndex of
        Just i ->
            if index == i - 1 then
                " is-before-active"

            else if index == i + 1 then
                " is-after-active"

            else
                ""

        Nothing ->
            ""


keyedBoardView : Bool -> Date -> Board -> ( String, Html Msg )
keyedBoardView ignoreFileNameDates today board =
    ( Board.id board, Html.Lazy.lazy3 boardView ignoreFileNameDates today board )


boardView : Bool -> Date -> Board -> Html Msg
boardView ignoreFileNameDates today board =
    Html.div
        [ class "card-board-board"
        , hidden True
        ]
        [ Html.Keyed.node "div"
            [ class "card-board-columns" ]
            (board
                |> Board.columns ignoreFileNameDates today
                |> List.indexedMap (\index column -> keyedColumnView Nothing (Board.id board) index today column)
            )
        ]



-- , Html.Keyed.node "div"
--     [ class "card-board-boards" ]
--     (Boards.boardZipper boards
--         |> SafeZipper.mapSelectedAndRest
--             (keyedSelectedBoardView ignoreFileNameDates today dragTracker)
--             (keyedBoardView ignoreFileNameDates today)
--         |> SafeZipper.toList
--     )


keyedSelectedBoardView : Bool -> Date -> DragTracker -> Board -> ( String, Html Msg )
keyedSelectedBoardView ignoreFileNameDates today dragTracker board =
    ( Board.id board, Html.Lazy.lazy4 selectedBoardView ignoreFileNameDates today dragTracker board )


selectedBoardView : Bool -> Date -> DragTracker -> Board -> Html Msg
selectedBoardView ignoreFileNameDates today dragTracker board =
    let
        draggedColumn : Maybe Column
        draggedColumn =
            board
                |> Board.columns ignoreFileNameDates today
                |> LE.find (\c -> Just (Column.name c) == draggedUniqueId)

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
            DragTracker.isDragging dragTracker && draggedType == Just columnDragType
    in
    Html.div [ class "card-board-board" ]
        [ Html.Keyed.node "div"
            [ class "card-board-columns" ]
            ((board
                |> Board.columns ignoreFileNameDates today
                |> List.indexedMap (\index column -> keyedColumnView draggedUniqueId (Board.id board) index today column)
             )
                |> (\cs ->
                        List.append cs
                            [ keyedColumnGhostView (Board.id board) today isDragging dragTracker draggedColumn ]
                   )
            )
        ]


keyedColumnGhostView : String -> Date -> Bool -> DragTracker -> Maybe Column -> ( String, Html Msg )
keyedColumnGhostView boardId today isDragging dragTracker draggedColumn =
    ( boardId ++ ":" ++ (Maybe.map Column.name draggedColumn |> Maybe.withDefault "") ++ ":ghost"
    , Html.Lazy.lazy5 columnGhostView boardId today isDragging dragTracker draggedColumn
    )


columnGhostView : String -> Date -> Bool -> DragTracker -> Maybe Column -> Html Msg
columnGhostView boardId today isDragging dragTracker draggedColumn =
    case ( isDragging, draggedColumn, dragTracker ) of
        ( True, Just column, DragTracker.Dragging clientData domData ) ->
            let
                columnCollapsedAria : String
                columnCollapsedAria =
                    if Column.isCollapsed column then
                        "Un-collapse"

                    else
                        "Collapse"

                columnCollapsedArrow : String
                columnCollapsedArrow =
                    if Column.isCollapsed column then
                        "arrow-down"

                    else
                        "arrow-right"

                columnCollapsedClass : String
                columnCollapsedClass =
                    if Column.isCollapsed column then
                        " collapsed"

                    else
                        ""

                columnCountString : String
                columnCountString =
                    if Column.isCollapsed column then
                        "(" ++ (String.fromInt <| Column.cardCount column) ++ ")"

                    else
                        ""

                name : String
                name =
                    Column.name column
            in
            Html.div []
                [ Html.div
                    [ class <| "card-board-column" ++ columnCollapsedClass
                    , style "position" "fixed"
                    , style "top" (String.fromFloat (domData.draggedNodeStartRect.y - domData.offset.y) ++ "px")
                    , style "left" (String.fromFloat (clientData.clientPos.x - domData.offset.x - clientData.offsetPos.x) ++ "px")
                    , style "width" (String.fromFloat domData.draggedNodeStartRect.width ++ "px")
                    , style "height" (String.fromFloat domData.draggedNodeStartRect.height ++ "px")
                    , style "cursor" "grabbing"
                    , style "opacity" "0.85"
                    ]
                    [ Html.div [ class "card-board-column-header" ]
                        [ Html.div
                            [ class columnCollapsedArrow
                            , attribute "aria-label" columnCollapsedAria
                            ]
                            []
                        , Html.span []
                            [ Html.text <| name ]
                        , Html.span [ class "sub-text" ]
                            [ Html.text <| columnCountString ]
                        ]
                    , Html.Keyed.ul [ class "card-board-column-list" ]
                        (List.map (cardView today) (Column.cards boardId column))
                    ]
                ]

        _ ->
            Html.text ""


keyedColumnView : Maybe String -> String -> Int -> Date -> Column -> ( String, Html Msg )
keyedColumnView draggedId boardId columnIndex today column =
    ( boardId ++ ":" ++ Column.name column
    , Html.Lazy.lazy5 columnView draggedId boardId columnIndex today column
    )


columnView : Maybe String -> String -> Int -> Date -> Column -> Html Msg
columnView draggedId boardId columnIndex today column =
    let
        columnCollapsedAria : String
        columnCollapsedAria =
            if Column.isCollapsed column then
                "Un-collapse"

            else
                "Collapse"

        columnCollapsedArrow : String
        columnCollapsedArrow =
            if Column.isCollapsed column then
                "arrow-down"

            else
                "arrow-right"

        columnCollapsedClass : String
        columnCollapsedClass =
            if Column.isCollapsed column then
                " collapsed"

            else
                ""

        columnCountString : String
        columnCountString =
            if Column.isCollapsed column then
                "(" ++ (String.fromInt <| Column.cardCount column) ++ ")"

            else
                ""

        domId : String
        domId =
            "card-board-column:"
                ++ boardId
                ++ ":"
                ++ name

        isBeingDragged : Bool
        isBeingDragged =
            Just name == draggedId

        name : String
        name =
            Column.name column
    in
    Html.div []
        [ columnBeacon (BeaconPosition.Before name)
        , Html.div
            [ id domId
            , class <| "card-board-column" ++ columnCollapsedClass
            , attributeIf isBeingDragged (style "opacity" "0")
            , onDown <|
                \e ->
                    ColumnMouseDown <|
                        ( domId
                        , { uniqueId = name
                          , clientPos = Coords.fromFloatTuple e.clientPos
                          , offsetPos = Coords.fromFloatTuple e.offsetPos
                          }
                        )
            ]
            [ Html.div [ class "card-board-column-header" ]
                [ Html.div
                    [ class columnCollapsedArrow
                    , attribute "aria-label" columnCollapsedAria
                    , onClick <| ToggleColumnCollapse columnIndex (not <| Column.isCollapsed column)
                    ]
                    []
                , Html.span []
                    [ Html.text <| name ]
                , Html.span [ class "sub-text" ]
                    [ Html.text <| columnCountString ]
                ]
            , Html.Keyed.ul [ class "card-board-column-list" ]
                (List.map (cardView today) (Column.cards boardId column))
            ]
        , columnBeacon (BeaconPosition.After name)
        ]


cardView : Date -> Card -> ( String, Html Msg )
cardView today card =
    let
        cardId : String
        cardId =
            Card.id card

        dataTags : String
        dataTags =
            card
                |> Card.allTags
                |> TagList.toStrings
                |> List.map (String.replace "/" "-")
                |> String.join " "

        taskItem : TaskItem
        taskItem =
            Card.taskItem card

        taskItemId : String
        taskItemId =
            Card.taskItemId card

        highlightAreaClass : String
        highlightAreaClass =
            case Card.highlight today card of
                Card.HighlightCritical ->
                    "critical"

                Card.HighlightGood ->
                    "good"

                Card.HighlightImportant ->
                    "important"

                Card.HighlightNone ->
                    ""
    in
    Html.li
        [ class "card-board-card cm-s-obsidian"
        , attributeIf (not <| String.isEmpty dataTags) (attribute "data-tags" dataTags)
        ]
        [ Html.div [ class ("card-board-card-highlight-area " ++ highlightAreaClass) ]
            []
        , Html.div [ class "card-board-card-content-area" ]
            [ Html.input
                [ type_ "checkbox"
                , class "task-list-item-checkbox"
                , onClick <| TaskItemToggled taskItemId
                , checked <| TaskItem.isCompleted taskItem
                ]
                []
            , Html.div [ class "card-board-card-title", id cardId ]
                []
            , cardTagsView (Card.tagsId card)
                |> when (TaskItem.hasTags taskItem)
            , subtasksView (Card.descendantTasks card)
                |> when (TaskItem.hasSubtasks taskItem)
            , notesView (Card.notesId card)
                |> when (TaskItem.hasNotes taskItem)
            , Html.div [ class "card-board-card-footer-area" ]
                [ taskDueDate today (TaskItem.due taskItem)
                    |> when (TaskItem.isDated taskItem)
                , cardActionButtons (Card.title card) taskItemId (Card.editButtonId card)
                ]
            ]
        ]
        |> Tuple.pair cardId


cardTagsView : String -> Html Msg
cardTagsView tagsId =
    Html.div [ class "card-board-card-tag-area", id tagsId ]
        []


notesView : String -> Html Msg
notesView notesId =
    Html.div [ class "card-board-card-notes-area", id notesId ]
        []


subtasksView : List ( String, TaskItem ) -> Html Msg
subtasksView subtasks =
    Html.div [ class "card-board-card-subtasks-area" ]
        [ Html.ul [ class "contains-task-list" ]
            (List.map subtaskView subtasks)
        ]


subtaskView : ( String, TaskItem ) -> Html Msg
subtaskView ( uniqueId, subtask ) =
    Html.li [ class "card-board-card-subtask task-list-item" ]
        [ Html.input
            [ type_ "checkbox"
            , class "task-list-item-checkbox"
            , onClick <| TaskItemToggled <| TaskItem.id subtask
            , checked <| TaskItem.isCompleted subtask
            ]
            []
        , Html.div [ class "card-board-card-title", id uniqueId ]
            []
        ]


taskDueDate : Date -> Maybe Date -> Html Msg
taskDueDate today dueDate =
    Html.div [ class "card-board-card-action-area-due" ]
        [ Html.text ("Due: " ++ dueDateString today dueDate)
        ]


dueDateString : Date -> Maybe Date -> String
dueDateString today dueDate =
    case dueDate of
        Just date ->
            if Date.year date == Date.year today then
                Date.format "E, MMM ddd" date

            else
                Date.format "E, MMM ddd y" date

        Nothing ->
            "n/a"


cardActionButtons : String -> String -> String -> Html Msg
cardActionButtons title taskItemId editButtonId =
    Html.div [ class "card-board-card-action-area-buttons" ]
        [ Html.div
            [ class "card-board-card-action-area-button"
            , onClickWithPreventDefault <| TaskItemEditClicked taskItemId
            , id editButtonId
            ]
            [ FeatherIcons.edit
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        , Html.div
            [ class "card-board-card-action-area-button"
            , onClick <| TaskItemDeleteClicked title taskItemId
            ]
            [ FeatherIcons.trash
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        ]



-- HELPERS


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


attributeIf : Bool -> Attribute msg -> Attribute msg
attributeIf condition attribute =
    if condition then
        attribute

    else
        class ""


columnBeaconType : String
columnBeaconType =
    "data-" ++ columnDragType ++ "-beacon"


columnBeacon : BeaconPosition -> Html Msg
columnBeacon beaconPosition =
    Html.span
        [ attribute columnBeaconType (JE.encode 0 <| BeaconPosition.encoder beaconPosition)
        , style "font-size" "0"
        ]
        []


empty : Html Msg
empty =
    Html.text ""


onClickWithPreventDefault : msg -> Attribute msg
onClickWithPreventDefault msg =
    Html.Events.preventDefaultOn "click" (JD.map alwaysPreventDefault (JD.succeed msg))


when : Bool -> Html Msg -> Html Msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty
