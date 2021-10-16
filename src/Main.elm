module Main exposing (main)

-- import DateBoard
-- import TagBoard

import Browser
import Card exposing (Card)
import CardBoard
import Date exposing (Date)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (checked, class, hidden, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import InteropDefinitions
import InteropPorts
import Json.Decode as JD
import MarkdownFile exposing (MarkdownFile)
import Panel exposing (Panel)
import Panels exposing (Panels)
import SafeZipper exposing (SafeZipper)
import Task
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- TYPES


type alias Model =
    { dailyNotesFolder : String
    , dailyNotesFormat : String
    , now : Time.Posix
    , zone : Time.Zone
    , taskList : State TaskList
    , boardConfigs : SafeZipper CardBoard.Config
    }


type State a
    = Loading
    | Loaded a


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        boardConfigs =
            [ CardBoard.DateBoardConfig
                { includeUndated = True
                , includeCompleted = True
                , title = "By Date"
                }
            , CardBoard.TagBoardConfig
                { columns =
                    [ { tag = "home", displayTitle = "Home Alone" }
                    , { tag = "home/", displayTitle = "All Home" }
                    , { tag = "town", displayTitle = "Town" }
                    , { tag = "wellbeing", displayTitle = "Wellbeing" }
                    ]
                , includeOthers = True
                , includeCompleted = True
                , title = "By Tag"
                }
            ]
    in
    case flags |> InteropPorts.decodeFlags of
        Err error ->
            Debug.todo <| Debug.toString error

        Ok okFlags ->
            ( { dailyNotesFolder = okFlags.folder
              , dailyNotesFormat = okFlags.format
              , now = Time.millisToPosix okFlags.now
              , zone = Time.customZone okFlags.zone []
              , taskList = Loading
              , boardConfigs = SafeZipper.fromList boardConfigs
              }
            , Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
            )



-- UPDATE


type Msg
    = BadInputFromTypeScript
    | ReceiveTime ( Time.Zone, Time.Posix )
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String
    | TaskItemToggled String
    | Tick Time.Posix
    | VaultFileAdded MarkdownFile
    | VaultFileDeleted String
    | VaultFileUpdated MarkdownFile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( BadInputFromTypeScript, _ ) ->
            ( model, Cmd.none )

        ( ReceiveTime ( zone, posix ), _ ) ->
            ( { model
                | zone = zone
                , now = posix
              }
            , Cmd.none
            )

        ( TabSelected tabIndex, _ ) ->
            ( { model | boardConfigs = SafeZipper.atIndex tabIndex model.boardConfigs }, Cmd.none )

        ( TaskItemDeleteClicked id, _ ) ->
            case model.taskList of
                Loaded taskList ->
                    case TaskList.taskFromId id taskList of
                        Just matchingItem ->
                            ( model
                            , InteropPorts.deleteTodo
                                { filePath = TaskItem.filePath matchingItem
                                , lineNumber = TaskItem.lineNumber matchingItem
                                , originalText = TaskItem.originalText matchingItem
                                }
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ( TaskItemEditClicked id, _ ) ->
            case model.taskList of
                Loaded taskList ->
                    case TaskList.taskFromId id taskList of
                        Just matchingItem ->
                            ( model
                            , InteropPorts.openTodoSourceFile
                                { filePath = TaskItem.filePath matchingItem
                                , blockLink = TaskItem.blockLink matchingItem
                                , lineNumber = TaskItem.lineNumber matchingItem
                                , originalText = TaskItem.originalText matchingItem
                                }
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ( TaskItemToggled id, _ ) ->
            case model.taskList of
                Loaded taskList ->
                    case TaskList.taskContainingId id taskList of
                        Just matchingItem ->
                            ( model
                            , InteropPorts.rewriteTodos
                                model.now
                                (TaskItem.filePath matchingItem)
                                (TaskItem.tasksToToggle id model.now matchingItem)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ( Tick time, _ ) ->
            ( { model | now = time }
            , Cmd.none
            )

        ( VaultFileAdded markdownFile, _ ) ->
            let
                newTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents

                cards =
                    newTaskItems
                        |> Panels.init model.boardConfigs
                        |> Panels.cards model.now model.zone
            in
            ( addTaskItems model newTaskItems
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown markdownFile.filePath cards
                , InteropPorts.addHoverToCardEditButtons markdownFile.filePath cards
                ]
            )

        ( VaultFileDeleted filePath, _ ) ->
            ( deleteItemsFromFile model filePath, Cmd.none )

        ( VaultFileUpdated markdownFile, _ ) ->
            let
                updatedTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents

                cards =
                    updatedTaskItems
                        |> Panels.init model.boardConfigs
                        |> Panels.cards model.now model.zone
            in
            ( updateTaskItems model markdownFile.filePath updatedTaskItems
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown markdownFile.filePath cards
                , InteropPorts.addHoverToCardEditButtons markdownFile.filePath cards
                ]
            )


deleteItemsFromFile : Model -> String -> Model
deleteItemsFromFile model filePath =
    case model.taskList of
        Loading ->
            model

        Loaded currentList ->
            { model | taskList = Loaded (TaskList.removeForFile filePath currentList) }


addTaskItems : Model -> TaskList -> Model
addTaskItems model taskList =
    case model.taskList of
        Loading ->
            { model | taskList = Loaded taskList }

        Loaded currentList ->
            { model | taskList = Loaded (TaskList.append currentList taskList) }


updateTaskItems : Model -> String -> TaskList -> Model
updateTaskItems model filePath updatedList =
    case model.taskList of
        Loading ->
            { model | taskList = Loaded updatedList }

        Loaded currentList ->
            { model | taskList = Loaded (TaskList.replaceForFile filePath updatedList currentList) }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , InteropPorts.toElm
            |> Sub.map
                (\result ->
                    case result of
                        Ok toElm ->
                            case toElm of
                                InteropDefinitions.FileAdded markdownFile ->
                                    VaultFileAdded markdownFile

                                InteropDefinitions.FileDeleted filePath ->
                                    VaultFileDeleted filePath

                                InteropDefinitions.FileUpdated markdownFile ->
                                    VaultFileUpdated markdownFile

                        Err error ->
                            BadInputFromTypeScript
                )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.taskList of
        Loaded taskList ->
            let
                panels =
                    Panels.init model.boardConfigs taskList
            in
            Html.div [ class "card-board" ]
                [ Html.div [ class "card-board-container" ]
                    [ Html.ul [ class "card-board-tab-list" ]
                        (Panels.tabTitles panels
                            |> SafeZipper.indexedMapSelectedAndRest selectedTabHeader tabHeader
                            |> SafeZipper.toList
                        )
                    , Html.div [ class "card-board-panels" ]
                        (Panels.panels panels
                            |> SafeZipper.indexedMapSelectedAndRest (selectedPanelView model) (panelView model)
                            |> SafeZipper.toList
                        )
                    ]
                ]

        _ ->
            Html.text ""


selectedTabHeader : Int -> String -> Html Msg
selectedTabHeader index title =
    Html.li [ class "card-board-title" ]
        [ Html.text <| title ++ " *" ]


tabHeader : Int -> String -> Html Msg
tabHeader index title =
    Html.li
        [ class "card-board-title"
        , onClick <| TabSelected index
        ]
        [ Html.text <| title ]


panelView : Model -> Int -> Panel -> Html Msg
panelView model index panel =
    Html.div
        [ class "card-board-panel"
        , hidden True
        ]
        [ Html.div [ class "card-board-columns" ]
            (panel
                |> Panel.columns model.now model.zone index
                |> List.map (\( n, cs ) -> column model.now model.zone n cs)
            )
        ]


selectedPanelView : Model -> Int -> Panel -> Html Msg
selectedPanelView model index panel =
    Html.div [ class "card-board-panel" ]
        [ Html.div [ class "card-board-columns" ]
            (panel
                |> Panel.columns model.now model.zone index
                |> List.map (\( n, cs ) -> column model.now model.zone n cs)
            )
        ]


column : Time.Posix -> Time.Zone -> String -> List Card -> Html Msg
column now zone title cards =
    Html.div [ class "card-board-column" ]
        [ Html.div [ class "card-board-column-header" ]
            [ Html.text title ]
        , Html.Keyed.ul [ class "card-board-column-list" ]
            (List.map (cardView now zone) cards)
        ]


cardView : Time.Posix -> Time.Zone -> Card -> ( String, Html Msg )
cardView now zone card =
    let
        uniqueId =
            Card.id card

        taskItem =
            Card.taskItem card

        highlightAreaClass =
            case Card.highlight now zone card of
                Card.HighlightCritical ->
                    "critical"

                Card.HighlightGood ->
                    "good"

                Card.HighlightImportant ->
                    "important"

                Card.HighlightNone ->
                    ""
    in
    Html.li [ class "card-board-card cm-s-obsidian markdown-preview-view" ]
        [ Html.div [ class ("card-board-card-highlight-area " ++ highlightAreaClass) ]
            []
        , Html.div [ class "card-board-card-content-area" ]
            [ Html.input
                [ type_ "checkbox"
                , class "task-list-item-checkbox"
                , onClick <| TaskItemToggled <| TaskItem.id taskItem
                , checked <| TaskItem.isCompleted taskItem
                ]
                []
            , Html.div [ class "card-board-card-title", id uniqueId ]
                []
            , cardTagsView taskItem
                |> when (TaskItem.hasTags taskItem)
            , subtasksView card
                |> when (TaskItem.hasSubtasks taskItem)
            , notesView card
                |> when (TaskItem.hasNotes taskItem)
            , Html.div [ class "card-board-card-footer-area" ]
                [ cardDueDate taskItem
                    |> when (TaskItem.isDated taskItem)
                , cardActionButtons card
                ]
            ]
        ]
        |> Tuple.pair uniqueId


cardTagsView : TaskItem -> Html Msg
cardTagsView taskItem =
    Html.div [ class "card-board-card-tag-area" ]
        (List.map cardTagView (TaskItem.tags taskItem))


cardTagView : String -> Html Msg
cardTagView tagText =
    Html.div [ class "card-board-card-tag" ]
        [ Html.span [ class "cm-hashtag-begin cm-hashtag" ]
            [ Html.text "#" ]
        , Html.span [ class "cm-list-1 cm-hashtag cm-hashtag-end" ]
            [ Html.text tagText ]
        , Html.span [ class "cm-list-1" ]
            [ Html.text " " ]
        ]


notesView : Card -> Html Msg
notesView card =
    let
        uniqueId =
            Card.id card ++ ":notes"
    in
    Html.div [ class "card-board-card-notes-area", id uniqueId ]
        []


subtasksView : Card -> Html Msg
subtasksView card =
    Html.div [ class "card-board-card-subtasks-area" ]
        [ Html.ul [ class "contains-task-list" ]
            (List.map subtaskView (Card.subtasks card))
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


cardDueDate : TaskItem -> Html Msg
cardDueDate taskItem =
    Html.div [ class "card-board-card-action-area-due" ]
        [ Html.text ("Due: " ++ dueDateString taskItem)
        ]


dueDateString : TaskItem -> String
dueDateString taskItem =
    case TaskItem.due taskItem of
        Just dueDate ->
            Date.format "E, MMM ddd" dueDate

        Nothing ->
            "n/a"


cardActionButtons : Card -> Html Msg
cardActionButtons card =
    let
        uniqueId =
            Card.id card ++ ":editButton"

        taskItem =
            Card.taskItem card
    in
    Html.div [ class "card-board-card-action-area-buttons" ]
        [ Html.div
            [ class "card-board-card-action-area-button"
            , onClick <| TaskItemEditClicked <| TaskItem.id taskItem
            , id uniqueId
            ]
            [ FeatherIcons.edit
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        , Html.div
            [ class "card-board-card-action-area-button"
            , onClick <| TaskItemDeleteClicked <| TaskItem.id taskItem
            ]
            [ FeatherIcons.trash
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        ]


empty : Html Msg
empty =
    Html.text ""


when : Bool -> Html Msg -> Html Msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty
