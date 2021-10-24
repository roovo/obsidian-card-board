module Main exposing (main)

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
import TimeWithZone exposing (TimeWithZone)


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
    , timeWithZone : TimeWithZone
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
                    [ { tag = "people/", displayTitle = "People" }
                    , { tag = "hydra/", displayTitle = "Hydra" }
                    , { tag = "finance/", displayTitle = "Finance" }
                    , { tag = "POC/", displayTitle = "ITT POC" }
                    ]
                , includeOthers = False
                , includeCompleted = False
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
              , timeWithZone =
                    { now = Time.millisToPosix okFlags.now
                    , zone = Time.customZone okFlags.zone []
                    }
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
                | timeWithZone =
                    { zone = zone
                    , now = posix
                    }
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
                                model.timeWithZone
                                (TaskItem.filePath matchingItem)
                                (TaskItem.tasksToToggle id model.timeWithZone matchingItem)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ( Tick time, _ ) ->
            ( { model | timeWithZone = TimeWithZone.now time model.timeWithZone }
            , Cmd.none
            )

        ( VaultFileAdded markdownFile, _ ) ->
            let
                newTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents

                cards =
                    newTaskItems
                        |> Panels.init model.boardConfigs
                        |> Panels.cards model.timeWithZone
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
                        |> Panels.cards model.timeWithZone
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

                currentIndex =
                    Panels.currentIndex panels
            in
            Html.div [ class "card-board" ]
                [ Html.div [ class "card-board-container" ]
                    [ Html.ul [ class "card-board-tab-list" ]
                        (tabHeaders currentIndex panels)
                    , Html.div [ class "card-board-panels" ]
                        (Panels.panels panels
                            |> SafeZipper.indexedMapSelectedAndRest (selectedPanelView model.timeWithZone) (panelView model.timeWithZone)
                            |> SafeZipper.toList
                        )
                    ]
                ]

        _ ->
            Html.text ""


tabHeaders : Maybe Int -> Panels -> List (Html Msg)
tabHeaders currentIndex panels =
    let
        beforeHeaderClass =
            tabHeaderClass currentIndex -1

        beforeFirst =
            Html.li [ class <| "card-board-pre-tabs" ++ beforeHeaderClass ]
                [ Html.div [ class "card-board-tabs-inner" ]
                    [ Html.text "" ]
                ]

        afterHeaderClass =
            tabHeaderClass currentIndex (Panels.length panels)

        afterLast =
            Html.li [ class <| "card-board-post-tabs" ++ afterHeaderClass ]
                [ Html.div [ class "card-board-tabs-inner" ]
                    [ Html.text "" ]
                ]

        tabs =
            Panels.tabTitles panels
                |> SafeZipper.indexedMapSelectedAndRest selectedTabHeader (tabHeader currentIndex)
                |> SafeZipper.toList
    in
    beforeFirst :: List.append tabs [ afterLast ]


selectedTabHeader : Int -> String -> Html Msg
selectedTabHeader index title =
    Html.li [ class "card-board-tab-title is-active" ]
        [ Html.div [ class "card-board-tabs-inner" ]
            [ Html.text <| title ]
        ]


tabHeader : Maybe Int -> Int -> String -> Html Msg
tabHeader currentIndex index title =
    let
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


panelView : TimeWithZone -> Int -> Panel -> Html Msg
panelView timeWithZone index panel =
    Html.div
        [ class "card-board-panel"
        , hidden True
        ]
        [ Html.div [ class "card-board-columns" ]
            (panel
                |> Panel.columns timeWithZone index
                |> List.map (\( n, cs ) -> column timeWithZone n cs)
            )
        ]


selectedPanelView : TimeWithZone -> Int -> Panel -> Html Msg
selectedPanelView timeWithZone index panel =
    Html.div [ class "card-board-panel" ]
        [ Html.div [ class "card-board-columns" ]
            (panel
                |> Panel.columns timeWithZone index
                |> List.map (\( n, cs ) -> column timeWithZone n cs)
            )
        ]


column : TimeWithZone -> String -> List Card -> Html Msg
column timeWithZone title cards =
    Html.div [ class "card-board-column" ]
        [ Html.div [ class "card-board-column-header" ]
            [ Html.text title ]
        , Html.Keyed.ul [ class "card-board-column-list" ]
            (List.map (cardView timeWithZone) cards)
        ]


cardView : TimeWithZone -> Card -> ( String, Html Msg )
cardView timeWithZone card =
    let
        cardId =
            Card.id card

        taskItem =
            Card.taskItem card

        taskItemId =
            Card.taskItemId card

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
    Html.li [ class "card-board-card cm-s-obsidian markdown-preview-view" ]
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
            , subtasksView (Card.subtasks card)
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


cardTagsView : List String -> Html Msg
cardTagsView tags =
    Html.div [ class "card-board-card-tag-area" ]
        (List.map cardTagView tags)


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
            , onClick <| TaskItemEditClicked taskItemId
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


empty : Html Msg
empty =
    Html.text ""


when : Bool -> Html Msg -> Html Msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty
