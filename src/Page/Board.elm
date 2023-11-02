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
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (attribute, checked, class, hidden, id, style, type_)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse exposing (onDown)
import Html.Keyed
import InteropPorts
import Json.Decode as JD
import Json.Encode as JE
import SafeZipper
import Session exposing (Session)
import TaskItem exposing (TaskItem, TaskItemFields)
import TextDirection
import TimeWithZone exposing (TimeWithZone)


type CandidatePosition
    = Before Int
    | After Int



-- UPDATE


type Msg
    = SettingsClicked
    | TabHeaderMouseDown Int ( Float, Float )
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String
    | TaskItemToggled String
    | ToggleColumnCollapse Int Bool


beaconIdentifier : String
beaconIdentifier =
    "data-card-board-tag-header-beacon"


update : Msg -> Session -> ( Session, Cmd Msg, Session.Msg )
update msg session =
    case msg of
        SettingsClicked ->
            ( session
            , Cmd.none
            , Session.SettingsClicked
            )

        TabHeaderMouseDown tabIndex clientPos ->
            ( session
            , InteropPorts.trackDraggable beaconIdentifier clientPos
            , Session.TrackDraggable <| Session.TabHeader tabIndex
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

            timeWithZone : TimeWithZone
            timeWithZone =
                Session.timeWithZone session
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
                    (tabHeaders currentBoardIndex boards)
                , Html.div
                    [ class "card-board-tab-header-spacer" ]
                    []
                ]
            , Html.div [ class "card-board-boards" ]
                (Boards.boardZipper boards
                    |> SafeZipper.indexedMapSelectedAndRest
                        (selectedBoardView ignoreFileNameDates timeWithZone)
                        (boardView ignoreFileNameDates timeWithZone)
                    |> SafeZipper.toList
                )
            ]


tabHeaders : Maybe Int -> Boards -> List (Html Msg)
tabHeaders currentBoardIndex boards =
    Boards.titles boards
        |> SafeZipper.indexedMapSelectedAndRest selectedTabHeader (tabHeader currentBoardIndex)
        |> SafeZipper.toList


selectedTabHeader : Int -> String -> Html Msg
selectedTabHeader tabIndex title =
    Html.div
        [ class "workspace-tab-header is-active"
        , id <| "card-board-tab:" ++ String.fromInt tabIndex
        , attribute "aria-label" title
        , attribute "aria-label-delay" "50"
        , onDown <| .clientPos >> TabHeaderMouseDown tabIndex
        ]
        [ beacon (Before tabIndex)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| title ]
            ]
        , beacon (After tabIndex)
        ]


tabHeader : Maybe Int -> Int -> String -> Html Msg
tabHeader currentBoardIndex tabIndex title =
    let
        headerClass : String
        headerClass =
            tabHeaderClass currentBoardIndex tabIndex
    in
    Html.div
        [ id <| "card-board-tab:" ++ String.fromInt tabIndex
        , class ("workspace-tab-header" ++ headerClass)
        , attribute "aria-label" title
        , attribute "aria-label-delay" "50"
        , onClick <| TabSelected tabIndex
        , onDown <| .clientPos >> TabHeaderMouseDown tabIndex
        ]
        [ beacon (Before tabIndex)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| title ]
            ]
        , beacon (After tabIndex)
        ]


beacon : CandidatePosition -> Html Msg
beacon candidatePosition =
    let
        positionValue : JE.Value
        positionValue =
            encodePostion candidatePosition
    in
    Html.span
        [ attribute beaconIdentifier (JE.encode 0 positionValue)
        , style "font-size" "0"
        ]
        []


encodePostion : CandidatePosition -> JE.Value
encodePostion candidatePosition =
    let
        ( positionStr, identifierString ) =
            case candidatePosition of
                After tabIndex ->
                    ( "after", String.fromInt tabIndex )

                Before tabIndex ->
                    ( "before", String.fromInt tabIndex )
    in
    JE.object
        [ ( "position", JE.string positionStr )
        , ( "identifier", JE.string identifierString )
        ]


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


boardView : Bool -> TimeWithZone -> Int -> Board -> Html Msg
boardView ignoreFileNameDates timeWithZone boardIndex board =
    Html.div
        [ class "card-board-board"
        , hidden True
        ]
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns ignoreFileNameDates timeWithZone boardIndex
                |> List.indexedMap (\index column -> columnView index timeWithZone column)
            )
        ]


selectedBoardView : Bool -> TimeWithZone -> Int -> Board -> Html Msg
selectedBoardView ignoreFileNameDates timeWithZone boardIndex board =
    Html.div [ class "card-board-board" ]
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns ignoreFileNameDates timeWithZone boardIndex
                |> List.indexedMap (\index column -> columnView index timeWithZone column)
            )
        ]


columnView : Int -> TimeWithZone -> Column Card -> Html Msg
columnView columnIndex timeWithZone column =
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
            (List.map (cardView timeWithZone) (Column.items column))
        ]


cardView : TimeWithZone -> Card -> ( String, Html Msg )
cardView timeWithZone card =
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
            case Card.highlight timeWithZone card of
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


onClickWithPreventDefault : msg -> Html.Attribute msg
onClickWithPreventDefault msg =
    Html.Events.preventDefaultOn "click" (JD.map alwaysPreventDefault (JD.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


empty : Html Msg
empty =
    Html.text ""


when : Bool -> Html Msg -> Html Msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty
