module Main exposing (main)

import Browser
import CardBoard exposing (CardBoard(..))
import Date exposing (Date)
import DateBoard
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (checked, class, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import Ports exposing (MarkdownFile)
import TagBoard
import Task
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time


main : Program Flags Model Msg
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
    , board : CardBoard
    }


type State a
    = Loading
    | Loaded a


type alias Flags =
    { folder : String
    , format : String
    , now : Int
    , zone : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { dailyNotesFolder = flags.folder
      , dailyNotesFormat = flags.format
      , now = Time.millisToPosix flags.now
      , zone = Time.customZone flags.zone []
      , taskList = Loading

      -- , board = Dated DateBoard.fill
      , board =
            Tagged <|
                TagBoard.fill
                    { columns = [ "Home", "Home/", "Town", "Wellbeing" ]
                    , includeOthers = True
                    , includeCompleted = True
                    }
      }
    , Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
    )



-- UPDATE


type Msg
    = ReceiveTime ( Time.Zone, Time.Posix )
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
        ( ReceiveTime ( zone, posix ), _ ) ->
            ( { model
                | zone = zone
                , now = posix
              }
            , Cmd.none
            )

        ( TaskItemDeleteClicked id, _ ) ->
            case model.taskList of
                Loaded taskList ->
                    case TaskList.taskFromId id taskList of
                        Just matchingItem ->
                            ( model
                            , Ports.deleteTodo
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
                            , Ports.openTodoSourceFile
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
                            , Ports.rewriteTodos
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
            in
            ( addTaskItems model newTaskItems
            , Cmd.batch
                [ Ports.displayTaskMarkdown markdownFile.filePath model.board model.now model.zone newTaskItems
                , Ports.addHoverToCardEditButtons markdownFile.filePath model.board model.now model.zone newTaskItems
                ]
            )

        ( VaultFileDeleted filePath, _ ) ->
            ( deleteItemsFromFile model filePath, Cmd.none )

        ( VaultFileUpdated markdownFile, _ ) ->
            let
                newTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( updateTaskItems model markdownFile.filePath newTaskItems
            , Cmd.batch
                [ Ports.displayTaskMarkdown markdownFile.filePath model.board model.now model.zone newTaskItems
                , Ports.addHoverToCardEditButtons markdownFile.filePath model.board model.now model.zone newTaskItems
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
        [ Ports.fileAdded VaultFileAdded
        , Ports.fileUpdated VaultFileUpdated
        , Ports.fileDeleted VaultFileDeleted
        , Time.every 1000 Tick
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.taskList of
        Loaded taskList ->
            Html.div [ class "card-board" ]
                [ Html.div [ class "card-board-container" ]
                    (model.board
                        |> CardBoard.columns model.now model.zone taskList
                        |> List.map (\( n, ts ) -> column n ts)
                    )
                ]

        _ ->
            Html.text ""


column : String -> List TaskItem -> Html Msg
column title taskItems =
    Html.div [ class "card-board-column" ]
        [ Html.div [ class "card-board-column-header" ]
            [ Html.text title ]
        , Html.Keyed.ul [ class "card-board-column-list" ]
            (taskItems
                |> List.map (card title)
            )
        ]


card : String -> TaskItem -> ( String, Html Msg )
card _ taskItem =
    let
        uniqueId =
            TaskItem.inColumnId taskItem
    in
    Html.li [ class "card-board-card cm-s-obsidian markdown-preview-view" ]
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
        , subtasksView taskItem
            |> when (TaskItem.hasSubtasks taskItem)
        , notesView taskItem
            |> when (TaskItem.hasNotes taskItem)
        , Html.div [ class "card-board-card-footer-area" ]
            [ cardDueDate taskItem
                |> when (TaskItem.isDated taskItem)
            , cardActionButtons taskItem
            ]
        ]
        |> Tuple.pair uniqueId


cardTagsView : TaskItem -> Html Msg
cardTagsView taskItem =
    Html.div [ class "card-board-card-tag-area" ]
        (List.map cardTagView (TaskItem.tags taskItem))


notesView : TaskItem -> Html Msg
notesView taskItem =
    let
        uniqueId =
            TaskItem.inColumnId taskItem ++ ":notes"
    in
    Html.div [ class "card-board-card-notes-area", id uniqueId ]
        []


subtasksView : TaskItem -> Html Msg
subtasksView taskItem =
    Html.div [ class "card-board-card-subtasks-area" ]
        [ Html.ul [ class "contains-task-list" ]
            (List.map subtaskView (TaskItem.subtasks taskItem))
        ]


subtaskView : TaskItem -> Html Msg
subtaskView subtask =
    let
        uniqueId =
            TaskItem.inColumnId subtask
    in
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


cardActionButtons : TaskItem -> Html Msg
cardActionButtons taskItem =
    let
        uniqueId =
            TaskItem.inColumnId taskItem ++ ":editButton"
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


empty : Html msg
empty =
    Html.text ""


when : Bool -> Html msg -> Html msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty
