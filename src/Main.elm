module Main exposing (main)

import Browser
import Card exposing (Card)
import CardBoard
import Date exposing (Date)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (checked, class, hidden, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import InteropDefinitions
import InteropPorts
import Json.Decode as JD
import Json.Encode as JE
import MarkdownFile exposing (MarkdownFile)
import Panel exposing (Panel)
import Panels exposing (Panels)
import Parser
import SafeZipper exposing (SafeZipper)
import TagBoard
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
    { boardConfigs : SafeZipper CardBoard.Config
    , configBeingEdited : Maybe (SafeZipper CardBoard.Config)
    , taskList : State TaskList
    , timeWithZone : TimeWithZone
    }


type State a
    = Waiting
    | Loading a
    | Loaded a


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    case flags |> InteropPorts.decodeFlags of
        Err error ->
            Debug.todo <| Debug.toString error

        Ok okFlags ->
            ( { boardConfigs = SafeZipper.fromList okFlags.boardConfigs
              , configBeingEdited = Nothing
              , taskList = Waiting
              , timeWithZone =
                    { now = Time.millisToPosix okFlags.now
                    , zone = Time.customZone okFlags.zone []
                    }
              }
            , Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
            )


hasLoaded : State TaskList -> Bool
hasLoaded taskList =
    case taskList of
        Loaded _ ->
            True

        _ ->
            False



-- UPDATE


type Msg
    = BadInputFromTypeScript JD.Error
    | EnteredCompletedCount String
    | EnteredTags String
    | EnteredTitle String
    | InitCompleted
    | ModalCloseClicked
    | ReceiveTime ( Time.Zone, Time.Posix )
    | SettingsBoardNameClicked Int
    | SettingsClicked
    | SettingsUpdated (List CardBoard.Config)
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String
    | TaskItemToggled String
    | Tick Time.Posix
    | ToggleIncludeOthers
    | ToggleIncludeUndated
    | ToggleIncludeUntagged
    | VaultFileAdded MarkdownFile
    | VaultFileDeleted String
    | VaultFileUpdated MarkdownFile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( BadInputFromTypeScript error, _ ) ->
            Debug.todo <| Debug.toString error

        ( EnteredCompletedCount value, _ ) ->
            let
                newConfig : Maybe (SafeZipper CardBoard.Config)
                newConfig =
                    case model.configBeingEdited of
                        Just c ->
                            Just (SafeZipper.mapCurrent updateCompletedCount c)

                        Nothing ->
                            Nothing

                updateCompletedCount : CardBoard.Config -> CardBoard.Config
                updateCompletedCount config =
                    case ( config, String.toInt value ) of
                        ( CardBoard.DateBoardConfig dateBoardConfig, Just newCount ) ->
                            CardBoard.DateBoardConfig { dateBoardConfig | completedCount = newCount }

                        ( CardBoard.TagBoardConfig tagBoardConfig, Just newCount ) ->
                            CardBoard.TagBoardConfig { tagBoardConfig | completedCount = newCount }

                        _ ->
                            config
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ( EnteredTags tags, _ ) ->
            let
                newConfig : Maybe (SafeZipper CardBoard.Config)
                newConfig =
                    case model.configBeingEdited of
                        Just c ->
                            Just (SafeZipper.mapCurrent updateTags c)

                        Nothing ->
                            Nothing

                updateTags : CardBoard.Config -> CardBoard.Config
                updateTags config =
                    case config of
                        CardBoard.DateBoardConfig dateBoardConfig ->
                            config

                        CardBoard.TagBoardConfig tagBoardConfig ->
                            let
                                columnsConfig =
                                    Parser.run TagBoard.columnConfigsParser tags
                            in
                            case columnsConfig of
                                Ok parsedConfig ->
                                    CardBoard.TagBoardConfig { tagBoardConfig | columns = parsedConfig }

                                _ ->
                                    config
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ( EnteredTitle title, _ ) ->
            let
                newConfig : Maybe (SafeZipper CardBoard.Config)
                newConfig =
                    case model.configBeingEdited of
                        Just c ->
                            Just (SafeZipper.mapCurrent updateTitle c)

                        Nothing ->
                            Nothing

                updateTitle : CardBoard.Config -> CardBoard.Config
                updateTitle config =
                    case config of
                        CardBoard.DateBoardConfig dateBoardConfig ->
                            CardBoard.DateBoardConfig { dateBoardConfig | title = title }

                        CardBoard.TagBoardConfig tagBoardConfig ->
                            CardBoard.TagBoardConfig { tagBoardConfig | title = title }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ( InitCompleted, _ ) ->
            case model.taskList of
                Waiting ->
                    ( { model | taskList = Loaded TaskList.empty }, Cmd.none )

                Loading taskList ->
                    let
                        cards =
                            taskList
                                |> Panels.init model.boardConfigs
                                |> Panels.cards model.timeWithZone
                    in
                    ( { model | taskList = Loaded taskList }
                    , Cmd.batch
                        [ InteropPorts.displayTaskMarkdown cards
                        , InteropPorts.addHoverToCardEditButtons cards
                        ]
                    )

                Loaded taskList ->
                    ( model, Cmd.none )

        ( ModalCloseClicked, _ ) ->
            case model.configBeingEdited of
                Just config ->
                    ( { model | configBeingEdited = Nothing }
                    , InteropPorts.updateSettings config
                    )

                Nothing ->
                    ( { model | configBeingEdited = Nothing }
                    , Cmd.none
                    )

        ( ReceiveTime ( zone, posix ), _ ) ->
            ( { model
                | timeWithZone =
                    { zone = zone
                    , now = posix
                    }
              }
            , Cmd.none
            )

        ( SettingsBoardNameClicked index, _ ) ->
            case model.configBeingEdited of
                Just boardConfigs ->
                    ( { model | configBeingEdited = Just <| SafeZipper.atIndex index boardConfigs }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( SettingsClicked, _ ) ->
            ( { model | configBeingEdited = Just model.boardConfigs }, Cmd.none )

        ( SettingsUpdated newSettings, _ ) ->
            case model.taskList of
                Waiting ->
                    ( { model | boardConfigs = SafeZipper.fromList newSettings }, Cmd.none )

                Loading taskList ->
                    ( { model | boardConfigs = SafeZipper.fromList newSettings }, Cmd.none )

                Loaded taskList ->
                    let
                        newConfigs =
                            SafeZipper.fromList newSettings

                        cards =
                            taskList
                                |> Panels.init newConfigs
                                |> Panels.cards model.timeWithZone
                    in
                    ( { model | boardConfigs = newConfigs }
                    , Cmd.batch
                        [ InteropPorts.displayTaskMarkdown cards
                        , InteropPorts.addHoverToCardEditButtons cards
                        ]
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

                _ ->
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

                _ ->
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

                _ ->
                    ( model, Cmd.none )

        ( Tick time, _ ) ->
            ( { model | timeWithZone = TimeWithZone.now time model.timeWithZone }
            , Cmd.none
            )

        ( ToggleIncludeOthers, _ ) ->
            let
                newConfig : Maybe (SafeZipper CardBoard.Config)
                newConfig =
                    case model.configBeingEdited of
                        Just c ->
                            Just (SafeZipper.mapCurrent toggleIncludeOthers c)

                        Nothing ->
                            Nothing

                toggleIncludeOthers : CardBoard.Config -> CardBoard.Config
                toggleIncludeOthers config =
                    case config of
                        CardBoard.DateBoardConfig dateBoardConfig ->
                            config

                        CardBoard.TagBoardConfig tagBoardConfig ->
                            CardBoard.TagBoardConfig { tagBoardConfig | includeOthers = not tagBoardConfig.includeOthers }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ( ToggleIncludeUndated, _ ) ->
            let
                newConfig : Maybe (SafeZipper CardBoard.Config)
                newConfig =
                    case model.configBeingEdited of
                        Just c ->
                            Just (SafeZipper.mapCurrent toggleIncludeUndated c)

                        Nothing ->
                            Nothing

                toggleIncludeUndated : CardBoard.Config -> CardBoard.Config
                toggleIncludeUndated config =
                    case config of
                        CardBoard.DateBoardConfig dateBoardConfig ->
                            CardBoard.DateBoardConfig { dateBoardConfig | includeUndated = not dateBoardConfig.includeUndated }

                        CardBoard.TagBoardConfig tagBoardConfig ->
                            config
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

        ( ToggleIncludeUntagged, _ ) ->
            let
                newConfig : Maybe (SafeZipper CardBoard.Config)
                newConfig =
                    case model.configBeingEdited of
                        Just c ->
                            Just (SafeZipper.mapCurrent toggleIncludeUntagged c)

                        Nothing ->
                            Nothing

                toggleIncludeUntagged : CardBoard.Config -> CardBoard.Config
                toggleIncludeUntagged config =
                    case config of
                        CardBoard.DateBoardConfig dateBoardConfig ->
                            config

                        CardBoard.TagBoardConfig tagBoardConfig ->
                            CardBoard.TagBoardConfig { tagBoardConfig | includeUntagged = not tagBoardConfig.includeUntagged }
            in
            ( { model | configBeingEdited = newConfig }, Cmd.none )

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
            , if hasLoaded model.taskList then
                Cmd.batch
                    [ InteropPorts.displayTaskMarkdown cards
                    , InteropPorts.addHoverToCardEditButtons cards
                    ]

              else
                Cmd.none
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
            , if hasLoaded model.taskList then
                Cmd.batch
                    [ InteropPorts.displayTaskMarkdown cards
                    , InteropPorts.addHoverToCardEditButtons cards
                    ]

              else
                Cmd.none
            )


deleteItemsFromFile : Model -> String -> Model
deleteItemsFromFile model filePath =
    case model.taskList of
        Waiting ->
            model

        Loading currentList ->
            { model | taskList = Loading (TaskList.removeForFile filePath currentList) }

        Loaded currentList ->
            { model | taskList = Loaded (TaskList.removeForFile filePath currentList) }


addTaskItems : Model -> TaskList -> Model
addTaskItems model taskList =
    case model.taskList of
        Waiting ->
            { model | taskList = Loading taskList }

        Loading currentList ->
            { model | taskList = Loading (TaskList.append currentList taskList) }

        Loaded currentList ->
            { model | taskList = Loaded (TaskList.append currentList taskList) }


updateTaskItems : Model -> String -> TaskList -> Model
updateTaskItems model filePath updatedList =
    case model.taskList of
        Waiting ->
            { model | taskList = Loading updatedList }

        Loading currentList ->
            { model | taskList = Loading (TaskList.replaceForFile filePath updatedList currentList) }

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

                                InteropDefinitions.InitCompleted _ ->
                                    InitCompleted

                                InteropDefinitions.SettingsUpdated newSettings ->
                                    SettingsUpdated newSettings.boardConfigs

                        Err error ->
                            BadInputFromTypeScript error
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
                , modalView model.configBeingEdited
                ]

        _ ->
            Html.text "Loading tasks...."


modalView : Maybe (SafeZipper CardBoard.Config) -> Html Msg
modalView configsBeingEdited =
    case configsBeingEdited of
        Just configs ->
            Html.div [ class "modal-container" ]
                [ Html.div [ class "modal-bg" ] []
                , Html.div [ class "modal mod-settings" ]
                    [ Html.div
                        [ class "modal-close-button"
                        , onClick ModalCloseClicked
                        ]
                        []
                    , Html.div [ class "modal-content" ]
                        [ Html.div [ class "settings-menu vertical-tab-header" ]
                            (Html.div [ class "vertical-tab-header-group-title" ]
                                [ Html.text "Boards" ]
                                :: (configs
                                        |> SafeZipper.indexedMapSelectedAndRest settingTitleSelectedView settingTitleView
                                        |> SafeZipper.toList
                                   )
                            )
                        , settingsFormView <| SafeZipper.current configs
                        ]
                    ]
                ]

        Nothing ->
            Html.text ""


settingsFormView : Maybe CardBoard.Config -> Html Msg
settingsFormView boardConfig =
    case boardConfig of
        Just (CardBoard.DateBoardConfig config) ->
            let
                includeUndatedStyle =
                    if config.includeUndated then
                        " is-enabled"

                    else
                        ""
            in
            Html.div [ class "settings-form" ]
                [ Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Title" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "The name of this board" ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value config.title
                            , onInput EnteredTitle
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Include Undated" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Whether to include a colum for tasks with no due date" ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.div
                            [ class <| "checkbox-container" ++ includeUndatedStyle
                            , onClick ToggleIncludeUndated
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Completed Count" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "How many completed tasks to show.  Set to zero to disable the completed column altogether." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value <| String.fromInt config.completedCount
                            , onInput EnteredCompletedCount
                            ]
                            []
                        ]
                    ]
                ]

        Just (CardBoard.TagBoardConfig config) ->
            let
                includeOthersStyle =
                    if config.includeOthers then
                        " is-enabled"

                    else
                        ""

                includeUntaggedStyle =
                    if config.includeUntagged then
                        " is-enabled"

                    else
                        ""

                tagText =
                    config.columns
                        |> List.map (\c -> "#" ++ c.tag ++ " " ++ c.displayTitle)
                        |> String.join "\n"
            in
            Html.div [ class "settings-form" ]
                [ Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Title" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "The name of this board" ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value config.title
                            , onInput EnteredTitle
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Columns" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.div []
                                [ Html.text "The tags to use to define board columns." ]
                            , Html.div []
                                [ Html.text
                                    ("Each line should be a tag followed by the column heading.  "
                                        ++ "Add a trailing / to the tag to include tasks with any subtags in the column too.  "
                                        ++ "If you do not specify the heading it will be auto-generated from the tag."
                                    )
                                ]
                            ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.textarea
                            [ onInput EnteredTags
                            , placeholder "#tag1 Column heading\n#tag2/\n#tag3/subtag\ntag4"
                            ]
                            [ Html.text tagText ]
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Include Others" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Whether to include a colum for tasks with tags other than those specified" ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.div
                            [ class <| "checkbox-container" ++ includeOthersStyle
                            , onClick ToggleIncludeOthers
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Include Untagged" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Whether to include a colum for tasks with no tags" ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.div
                            [ class <| "checkbox-container" ++ includeUntaggedStyle
                            , onClick ToggleIncludeUntagged
                            ]
                            []
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Completed Count" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "How many completed tasks to show.  Set to zero to disable the completed column altogether." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ Html.input
                            [ type_ "text"
                            , value <| String.fromInt config.completedCount
                            , onInput EnteredCompletedCount
                            ]
                            []
                        ]
                    ]
                ]

        Nothing ->
            Html.text "nowt here"


settingTitleSelectedView : Int -> CardBoard.Config -> Html Msg
settingTitleSelectedView index boardConfig =
    Html.div
        [ class "vertical-tab-nav-item is-active"
        , onClick <| SettingsBoardNameClicked index
        ]
        [ Html.text <| CardBoard.title boardConfig ]


settingTitleView : Int -> CardBoard.Config -> Html Msg
settingTitleView index boardConfig =
    Html.div
        [ class "vertical-tab-nav-item"
        , onClick <| SettingsBoardNameClicked index
        ]
        [ Html.text <| CardBoard.title boardConfig ]


tabHeaders : Maybe Int -> Panels -> List (Html Msg)
tabHeaders currentIndex panels =
    let
        beforeHeaderClass =
            tabHeaderClass currentIndex -1

        beforeFirst =
            Html.li [ class <| "card-board-pre-tabs" ++ beforeHeaderClass ]
                [ Html.div
                    [ class "card-board-tabs-inner"
                    , onClick SettingsClicked
                    ]
                    [ FeatherIcons.settings
                        |> FeatherIcons.withSize 1
                        |> FeatherIcons.withSizeUnit "em"
                        |> FeatherIcons.toHtml []
                    ]
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
