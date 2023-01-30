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
import Html.Attributes exposing (attribute, checked, class, hidden, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import InteropPorts
import Json.Decode as JD
import SafeZipper
import Session exposing (Session)
import TaskItem exposing (TaskItem, TaskItemFields)
import TextDirection
import TimeWithZone exposing (TimeWithZone)



-- UPDATE


type Msg
    = SettingsClicked
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String
    | TaskItemToggled String
    | ToggleColumnCollapse Int Bool


update : Msg -> Session -> ( Session, Cmd Msg, Session.Msg )
update msg session =
    case msg of
        SettingsClicked ->
            ( session
            , Cmd.none
            , Session.SettingsClicked
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
            ( Session.updateColumnCollapse columnIndex newState session
            , Cmd.none
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
                Boards.init columnNames (Session.boardConfigs session) (Session.taskList session)

            columnNames : ColumnNames
            columnNames =
                Session.globalSettings session
                    |> .columnNames

            currentBoardIndex : Maybe Int
            currentBoardIndex =
                Boards.currentIndex boards

            timeWithZone : TimeWithZone
            timeWithZone =
                Session.timeWithZone session
        in
        Html.div [ attribute "dir" (TextDirection.toString <| Session.textDirection session) ]
            [ Html.ul [ class "card-board-tab-list" ]
                (tabHeaders currentBoardIndex boards)
            , Html.div [ class "card-board-boards" ]
                (Boards.boardZipper boards
                    |> SafeZipper.indexedMapSelectedAndRest (selectedBoardView timeWithZone) (boardView timeWithZone)
                    |> SafeZipper.toList
                )
            ]


tabHeaders : Maybe Int -> Boards -> List (Html Msg)
tabHeaders currentBoardIndex boards =
    let
        beforeHeaderClass : String
        beforeHeaderClass =
            tabHeaderClass currentBoardIndex -1

        beforeFirst : Html Msg
        beforeFirst =
            Html.li [ class <| "card-board-pre-tabs" ++ beforeHeaderClass ]
                [ Html.div
                    [ class "card-board-tabs-inner card-board-tab-icon"
                    , onClick SettingsClicked
                    ]
                    [ FeatherIcons.settings
                        |> FeatherIcons.withSize 1
                        |> FeatherIcons.withSizeUnit "em"
                        |> FeatherIcons.toHtml []
                    ]
                ]

        afterHeaderClass : String
        afterHeaderClass =
            tabHeaderClass currentBoardIndex (Boards.length boards)

        afterLast : Html Msg
        afterLast =
            Html.li [ class <| "card-board-post-tabs" ++ afterHeaderClass ]
                [ Html.div [ class "card-board-tabs-inner" ]
                    [ Html.text "" ]
                ]

        tabs : List (Html Msg)
        tabs =
            Boards.titles boards
                |> SafeZipper.indexedMapSelectedAndRest selectedTabHeader (tabHeader currentBoardIndex)
                |> SafeZipper.toList
    in
    beforeFirst :: List.append tabs [ afterLast ]


selectedTabHeader : Int -> String -> Html Msg
selectedTabHeader _ title =
    Html.li [ class "card-board-tab-title is-active" ]
        [ Html.div [ class "card-board-tabs-inner" ]
            [ Html.text <| title ]
        ]


tabHeader : Maybe Int -> Int -> String -> Html Msg
tabHeader currentBoardIndex index title =
    let
        headerClass : String
        headerClass =
            tabHeaderClass currentBoardIndex index
    in
    Html.li
        [ class ("card-board-tab-title" ++ headerClass)
        , onClick <| TabSelected index
        ]
        [ Html.div [ class "card-board-tabs-inner" ]
            [ Html.text <| title ]
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


boardView : TimeWithZone -> Int -> Board -> Html Msg
boardView timeWithZone boardIndex board =
    Html.div
        [ class "card-board-board"
        , hidden True
        ]
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns timeWithZone boardIndex
                |> List.indexedMap (\index column -> columnView index timeWithZone column)
            )
        ]


selectedBoardView : TimeWithZone -> Int -> Board -> Html Msg
selectedBoardView timeWithZone boardIndex board =
    Html.div [ class "card-board-board" ]
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns timeWithZone boardIndex
                |> List.indexedMap (\index column -> columnView index timeWithZone column)
            )
        ]


columnView : Int -> TimeWithZone -> Column Card -> Html Msg
columnView columnIndex timeWithZone column =
    let
        columnCollapsedArrow =
            if Column.isCollapsed column then
                "arrow-down"

            else
                "arrow-right"

        columnCollapsedClass =
            if Column.isCollapsed column then
                " collapsed"

            else
                ""

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
