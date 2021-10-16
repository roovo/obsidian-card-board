module Main exposing (main)

import Browser
import CardBoard
import Date exposing (Date)
import DateBoard
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (checked, class, hidden, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import InteropDefinitions
import InteropPorts
import Json.Decode as JD
import MarkdownFile exposing (MarkdownFile)
import SafeZipper exposing (SafeZipper)
import TagBoard
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
            in
            ( addTaskItems model newTaskItems
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown markdownFile.filePath model.now model.zone newTaskItems
                , InteropPorts.addHoverToCardEditButtons markdownFile.filePath model.now model.zone newTaskItems
                ]
            )

        ( VaultFileDeleted filePath, _ ) ->
            ( deleteItemsFromFile model filePath, Cmd.none )

        ( VaultFileUpdated markdownFile, _ ) ->
            let
                updatedTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( updateTaskItems model markdownFile.filePath updatedTaskItems
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown markdownFile.filePath model.now model.zone updatedTaskItems
                , InteropPorts.addHoverToCardEditButtons markdownFile.filePath model.now model.zone updatedTaskItems
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
            Html.div [ class "card-board" ]
                [ Html.div [ class "card-board-container" ]
                    [ Html.ul
                        [ class "card-board-tab-list"
                        ]
                        (model.boardConfigs
                            |> SafeZipper.indexedMapSelectedAndRest selectedTabHeader tabHeader
                        )
                    , Html.div [ class "card-board-panels" ]
                        (model.boardConfigs
                            |> SafeZipper.indexedMapSelectedAndRest (selectedPanel model taskList) (panel model taskList)
                        )
                    ]
                ]

        _ ->
            Html.text ""


tabHeader : Int -> CardBoard.Config -> Html Msg
tabHeader index config =
    Html.li
        [ class "card-board-title"
        , onClick <| TabSelected index
        ]
        [ Html.text <| CardBoard.title config ]


selectedTabHeader : Int -> CardBoard.Config -> Html Msg
selectedTabHeader index config =
    Html.li [ class "card-board-title" ]
        [ Html.text <| CardBoard.title config ++ " *" ]


panel : Model -> TaskList -> Int -> CardBoard.Config -> Html Msg
panel model taskList index config =
    Html.div
        [ class "card-board-panel"
        , hidden True
        ]
        [ Html.div [ class "card-board-columns" ]
            (config
                |> CardBoard.columns model.now model.zone taskList
                |> List.map (\( n, ts ) -> column model.now model.zone n ts)
            )
        ]


selectedPanel : Model -> TaskList -> Int -> CardBoard.Config -> Html Msg
selectedPanel model taskList index config =
    Html.div [ class "card-board-panel" ]
        [ Html.div [ class "card-board-columns" ]
            (config
                |> CardBoard.columns model.now model.zone taskList
                |> List.map (\( n, ts ) -> column model.now model.zone n ts)
            )
        ]


column : Time.Posix -> Time.Zone -> String -> List TaskItem -> Html Msg
column now zone title taskItems =
    Html.div [ class "card-board-column" ]
        [ Html.div [ class "card-board-column-header" ]
            [ Html.text title ]
        , Html.Keyed.ul [ class "card-board-column-list" ]
            (taskItems
                |> List.map (card now zone)
            )
        ]


card : Time.Posix -> Time.Zone -> TaskItem -> ( String, Html Msg )
card now zone taskItem =
    let
        uniqueId =
            TaskItem.inColumnId taskItem

        highlightAreaClass =
            case TaskItem.highlight now zone taskItem of
                TaskItem.HighlightCritical ->
                    "critical"

                TaskItem.HighlightGood ->
                    "good"

                TaskItem.HighlightImportant ->
                    "important"

                TaskItem.HighlightNone ->
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
