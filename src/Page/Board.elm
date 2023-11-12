module Page.Board exposing
    ( Msg(..)
    , update
    , view
    )

import Board exposing (Board)
import Boards exposing (Boards)
import Card exposing (Card)
import Column exposing (Column)
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords
import DragAndDrop.DragData as DragData exposing (DragData, DragTracker)
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
import SafeZipper
import Session exposing (Session)
import TaskItem exposing (TaskItem, TaskItemFields)
import TextDirection
import TimeWithZone exposing (TimeWithZone)



-- UPDATE


type Msg
    = ElementDragged DragData
    | SettingsClicked
    | TabHeaderMouseDown DragTracker
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String
    | TaskItemToggled String
    | ToggleColumnCollapse Int Bool


beaconIdentifier : String
beaconIdentifier =
    "data-card-board-tag-header-beacon"


updateBoardOrder : DragTracker -> DragData -> Session -> Session
updateBoardOrder { nodeId } { cursor, beacons } session =
    case Rect.closestTo cursor beacons of
        Nothing ->
            session

        Just position ->
            Session.moveBoard nodeId position session


update : Msg -> Session -> ( Session, Cmd Msg, Session.Msg )
update msg session =
    case msg of
        ElementDragged dragData ->
            case dragData.dragAction of
                DragData.Move ->
                    case Session.dragStatus session of
                        Session.Dragging dragTracker ->
                            ( updateBoardOrder dragTracker dragData session
                            , Cmd.none
                            , Session.NoOp
                            )

                        Session.NotDragging ->
                            ( session
                            , Cmd.none
                            , Session.NoOp
                            )

                DragData.Stop ->
                    ( Session.stopTrackingDragable session
                    , Cmd.none
                    , Session.NoOp
                    )

        SettingsClicked ->
            ( session
            , Cmd.none
            , Session.SettingsClicked
            )

        TabHeaderMouseDown dragTracker ->
            ( session
            , InteropPorts.trackDraggable beaconIdentifier dragTracker.clientPos
            , Session.TrackDraggable dragTracker
            )

        TabSelected tabIndex ->
            ( Session.switchToBoardAt tabIndex session
            , Cmd.none
            , Session.NoOp
            )

        TaskItemDeleteClicked id ->
            ( session
            , cmdIfHasTask id session InteropPorts.deleteTask
            , Session.NoOp
            )

        TaskItemEditClicked id ->
            ( session
            , cmdIfHasTask id session InteropPorts.openTaskSourceFile
            , Session.NoOp
            )

        TaskItemToggled id ->
            let
                timeWithZone : TimeWithZone
                timeWithZone =
                    Session.timeWithZone session

                toggleCmd : TaskItem -> Cmd Msg
                toggleCmd taskItem =
                    InteropPorts.rewriteTasks
                        (Session.dataviewTaskCompletion session)
                        (Session.globalSettings session |> .taskCompletionFormat)
                        timeWithZone
                        (TaskItem.filePath taskItem)
                        (TaskItem.tasksToToggle id timeWithZone taskItem)

                cmd : Cmd Msg
                cmd =
                    session
                        |> Session.taskContainingId id
                        |> Maybe.map toggleCmd
                        |> Maybe.withDefault Cmd.none
            in
            ( session
            , cmd
            , Session.NoOp
            )

        ToggleColumnCollapse columnIndex newState ->
            let
                newSession : Session
                newSession =
                    Session.updateColumnCollapse columnIndex newState session
            in
            ( newSession
            , InteropPorts.updateSettings <| Session.settings newSession
            , Session.NoOp
            )


cmdIfHasTask : String -> Session -> (TaskItemFields -> Cmd b) -> Cmd b
cmdIfHasTask id session cmd =
    session
        |> Session.taskFromId id
        |> Maybe.map TaskItem.fields
        |> Maybe.map cmd
        |> Maybe.withDefault Cmd.none



-- VIEW


view : Session -> Html Msg
view session =
    if SafeZipper.length (Session.boardConfigs session) == 0 then
        Html.text "Loading tasks...."

    else
        let
            boards : Boards
            boards =
                Boards.init (Session.uniqueId session) columnNames (Session.boardConfigs session) (Session.taskList session)

            columnNames : ColumnNames
            columnNames =
                Session.globalSettings session
                    |> .columnNames

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
                case Session.dragStatus session of
                    Session.Dragging _ ->
                        True

                    Session.NotDragging ->
                        False
        in
        Html.div [ attribute "dir" (TextDirection.toString <| Session.textDirection session) ]
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
                    (tabHeaders isDragging currentBoardIndex boards)
                , Html.div
                    [ class "card-board-tab-header-spacer" ]
                    []
                ]
            , Html.Keyed.node "div"
                [ class "card-board-boards" ]
                (Boards.boardZipper boards
                    |> SafeZipper.mapSelectedAndRest
                        (keyedSelectedBoardView ignoreFileNameDates today)
                        (keyedBoardView ignoreFileNameDates today)
                    |> SafeZipper.toList
                )
            ]


tabHeaders : Bool -> Maybe Int -> Boards -> List (Html Msg)
tabHeaders isDragging currentBoardIndex boards =
    Boards.titles boards
        |> SafeZipper.indexedMapSelectedAndRest (selectedTabHeader isDragging) (tabHeader isDragging currentBoardIndex)
        |> SafeZipper.toList


selectedTabHeader : Bool -> Int -> String -> Html Msg
selectedTabHeader isDragging tabIndex title =
    Html.div
        [ class "workspace-tab-header is-active"
        , id <| "card-board-tab:" ++ String.fromInt tabIndex
        , attributeIf (not isDragging) (attribute "aria-label" title)
        , attributeIf (not isDragging) (attribute "aria-label-delay" "50")
        , onDown
            (\e ->
                TabHeaderMouseDown <|
                    DragTracker title
                        (Coords.fromFloatTuple e.clientPos)
                        (Coords.fromFloatTuple e.offsetPos)
            )
        ]
        [ beacon (BeaconPosition.Before title)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| title ]
            ]
        , beacon (BeaconPosition.After title)
        ]


tabHeader : Bool -> Maybe Int -> Int -> String -> Html Msg
tabHeader isDragging currentBoardIndex tabIndex title =
    let
        headerClass : String
        headerClass =
            tabHeaderClass currentBoardIndex tabIndex
    in
    Html.div
        [ id <| "card-board-tab:" ++ String.fromInt tabIndex
        , class ("workspace-tab-header" ++ headerClass)
        , attributeIf (not isDragging) (attribute "aria-label" title)
        , attributeIf (not isDragging) (attribute "aria-label-delay" "50")
        , onClick <| TabSelected tabIndex
        , onDown
            (\e ->
                TabHeaderMouseDown <|
                    DragTracker title
                        (Coords.fromFloatTuple e.clientPos)
                        (Coords.fromFloatTuple e.offsetPos)
            )
        ]
        [ beacon (BeaconPosition.Before title)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| title ]
            ]
        , beacon (BeaconPosition.After title)
        ]


beacon : BeaconPosition -> Html Msg
beacon beaconPosition =
    Html.span
        [ attribute beaconIdentifier (JE.encode 0 <| BeaconPosition.encoder beaconPosition)
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
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns ignoreFileNameDates today
                |> List.indexedMap (\index column -> columnView index today column)
            )
        ]


keyedSelectedBoardView : Bool -> Date -> Board -> ( String, Html Msg )
keyedSelectedBoardView ignoreFileNameDates today board =
    ( Board.id board, Html.Lazy.lazy3 selectedBoardView ignoreFileNameDates today board )


selectedBoardView : Bool -> Date -> Board -> Html Msg
selectedBoardView ignoreFileNameDates today board =
    Html.div [ class "card-board-board" ]
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns ignoreFileNameDates today
                |> List.indexedMap (\index column -> columnView index today column)
            )
        ]


columnView : Int -> Date -> Column Card -> Html Msg
columnView columnIndex today column =
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
                "(" ++ (String.fromInt <| List.length <| Column.items column) ++ ")"

            else
                ""
    in
    Html.div [ class <| "card-board-column" ++ columnCollapsedClass ]
        [ Html.div [ class "card-board-column-header" ]
            [ Html.div
                [ class columnCollapsedArrow
                , attribute "aria-label" columnCollapsedAria
                , onClick <| ToggleColumnCollapse columnIndex (not <| Column.isCollapsed column)
                ]
                []
            , Html.span []
                [ Html.text <| Column.name column ]
            , Html.span [ class "sub-text" ]
                [ Html.text <| columnCountString ]
            ]
        , Html.Keyed.ul [ class "card-board-column-list" ]
            (List.map (cardView today) (Column.items column))
        ]


cardView : Date -> Card -> ( String, Html Msg )
cardView today card =
    let
        cardId : String
        cardId =
            Card.id card

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
    Html.li [ class "card-board-card cm-s-obsidian" ]
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
                [ taskDueDate (TaskItem.due taskItem)
                    |> when (TaskItem.isDated taskItem)
                , cardActionButtons taskItemId (Card.editButtonId card)
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


taskDueDate : Maybe Date -> Html Msg
taskDueDate dueDate =
    Html.div [ class "card-board-card-action-area-due" ]
        [ Html.text ("Due: " ++ dueDateString dueDate)
        ]


dueDateString : Maybe Date -> String
dueDateString dueDate =
    case dueDate of
        Just date ->
            Date.format "E, MMM ddd" date

        Nothing ->
            "n/a"


cardActionButtons : String -> String -> Html Msg
cardActionButtons taskItemId editButtonId =
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
            , onClick <| TaskItemDeleteClicked taskItemId
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
        Html.Attributes.class ""


empty : Html Msg
empty =
    Html.text ""


onClickWithPreventDefault : msg -> Html.Attribute msg
onClickWithPreventDefault msg =
    Html.Events.preventDefaultOn "click" (JD.map alwaysPreventDefault (JD.succeed msg))


when : Bool -> Html Msg -> Html Msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty
