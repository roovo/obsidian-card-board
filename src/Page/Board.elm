module Page.Board exposing
    ( Msg(..)
    , update
    , view
    )

import Board exposing (Board)
import Boards exposing (Boards)
import Card exposing (Card)
import Column exposing (Column)
import Date exposing (Date)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (checked, class, hidden, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import InteropPorts
import Json.Decode as JD
import SafeZipper
import Session exposing (Session)
import TagList exposing (TagList)
import TaskItem exposing (TaskItem, TaskItemFields)
import TimeWithZone exposing (TimeWithZone)



-- UPDATE


type Msg
    = SettingsClicked
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String
    | TaskItemToggled String


update : Msg -> Session -> ( Session, Cmd Msg, Session.Msg )
update msg session =
    case msg of
        SettingsClicked ->
            ( session
            , Cmd.none
            , Session.SettingsClicked
            )

        TabSelected tabIndex ->
            ( Session.mapConfig (\c -> { c | boardConfigs = SafeZipper.atIndex tabIndex (Session.boardConfigs session) }) session
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
                Boards.init (Session.boardConfigs session) (Session.currentTaskList session)

            currentIndex : Maybe Int
            currentIndex =
                Boards.currentIndex boards

            timeWithZone : TimeWithZone
            timeWithZone =
                Session.timeWithZone session
        in
        Html.div []
            [ Html.ul [ class "card-board-tab-list" ]
                (tabHeaders currentIndex boards)
            , Html.div [ class "card-board-boards" ]
                (Boards.boardZipper boards
                    |> SafeZipper.indexedMapSelectedAndRest (selectedBoardView timeWithZone) (boardView timeWithZone)
                    |> SafeZipper.toList
                )
            ]


tabHeaders : Maybe Int -> Boards -> List (Html Msg)
tabHeaders currentIndex boards =
    let
        beforeHeaderClass : String
        beforeHeaderClass =
            tabHeaderClass currentIndex -1

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
            tabHeaderClass currentIndex (Boards.length boards)

        afterLast : Html Msg
        afterLast =
            Html.li [ class <| "card-board-post-tabs" ++ afterHeaderClass ]
                [ Html.div [ class "card-board-tabs-inner" ]
                    [ Html.text "" ]
                ]

        tabs : List (Html Msg)
        tabs =
            Boards.titles boards
                |> SafeZipper.indexedMapSelectedAndRest selectedTabHeader (tabHeader currentIndex)
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
tabHeader currentIndex index title =
    let
        headerClass : String
        headerClass =
            tabHeaderClass currentIndex index
    in
    Html.li
        [ class ("card-board-tab-title" ++ headerClass)
        , onClick <| TabSelected index
        ]
        [ Html.div [ class "card-board-tabs-inner" ]
            [ Html.text <| title ]
        ]


tabHeaderClass : Maybe Int -> Int -> String
tabHeaderClass currentIndex index =
    case currentIndex of
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
boardView timeWithZone index board =
    Html.div
        [ class "card-board-board"
        , hidden True
        ]
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns timeWithZone index
                |> List.map (\column -> columnView timeWithZone column)
            )
        ]


selectedBoardView : TimeWithZone -> Int -> Board -> Html Msg
selectedBoardView timeWithZone index board =
    Html.div [ class "card-board-board" ]
        [ Html.div [ class "card-board-columns" ]
            (board
                |> Board.columns timeWithZone index
                |> List.map (\column -> columnView timeWithZone column)
            )
        ]


columnView : TimeWithZone -> Column Card -> Html Msg
columnView timeWithZone column =
    Html.div [ class "card-board-column" ]
        [ Html.div [ class "card-board-column-header" ]
            [ Html.text <| Column.name column ]
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
            , cardTagsView (TaskItem.tags taskItem)
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


cardTagsView : TagList -> Html Msg
cardTagsView tags =
    Html.div [ class "card-board-card-tag-area" ]
        (List.map cardTagView <| TagList.toList tags)


cardTagView : String -> Html Msg
cardTagView tagText =
    Html.div [ class ("card-board-card-tag tag-" ++ String.replace "/" "-" tagText) ]
        [ Html.span [ class "cm-hashtag-begin cm-hashtag" ]
            [ Html.text "#" ]
        , Html.span [ class "cm-list-1 cm-hashtag cm-hashtag-end" ]
            [ Html.text tagText ]
        , Html.span [ class "cm-list-1" ]
            [ Html.text " " ]
        ]


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
