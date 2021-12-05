module Main exposing (main)

import BoardConfig exposing (BoardConfig)
import Boards
import Browser
import Browser.Events as Browser
import Filter exposing (Filter)
import Html exposing (Html)
import InteropDefinitions
import InteropPorts
import Json.Decode as JD
import MarkdownFile exposing (MarkdownFile)
import Page
import Page.Board as BoardPage
import Page.Settings as SettingsPage
import SafeZipper
import Session exposing (Session)
import State exposing (State)
import Task
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time
import TimeWithZone


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    case flags |> InteropPorts.decodeFlags of
        Err _ ->
            ( Settings (SettingsPage.init Session.default)
            , Cmd.batch
                [ Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
                , InteropPorts.elmInitialized
                ]
            )

        Ok okFlags ->
            let
                session =
                    Session.fromFlags okFlags
            in
            ( Boards session
                |> forceAddWhenNoBoards
            , Cmd.batch
                [ InteropPorts.updateSettings (Session.boardConfigs session)
                , InteropPorts.elmInitialized
                , Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
                ]
            )


forceAddWhenNoBoards : Model -> Model
forceAddWhenNoBoards model =
    case model of
        Boards session ->
            if SafeZipper.length (Session.boardConfigs session) == 0 then
                Settings (SettingsPage.init session)

            else
                model

        _ ->
            model



-- MODEL


type Model
    = Boards Session
    | Settings SettingsPage.Model
    | StartupError Session


mapSessionConfig : (Session.Config -> Session.Config) -> Model -> Model
mapSessionConfig fn model =
    case model of
        Boards session ->
            Boards <| Session.mapConfig fn session

        Settings settingsPageModel ->
            Settings <| SettingsPage.mapSessionConfig fn settingsPageModel

        StartupError session ->
            StartupError <| Session.mapConfig fn session


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    case model of
        Boards session ->
            Boards <| fn session

        Settings settingsPageModel ->
            Settings <| SettingsPage.mapSession fn settingsPageModel

        StartupError session ->
            StartupError <| fn session


toSession : Model -> Session
toSession model =
    case model of
        Boards session ->
            session

        Settings settingsPageModel ->
            SettingsPage.toSession settingsPageModel

        StartupError session ->
            session



-- UPDATE


type KeyValue
    = Character Char
    | Control String


type Msg
    = ActiveStateUpdated Bool
    | AllMarkdownLoaded
    | BadInputFromTypeScript
    | BoardConfigsUpdated (List BoardConfig)
    | FolderPathsReceived (List Filter)
    | GotBoardPageMsg BoardPage.Msg
    | GotSettingsPageMsg SettingsPage.Msg
    | KeyDown KeyValue
    | ReceiveTime ( Time.Zone, Time.Posix )
    | ShowBoard Int
    | Tick Time.Posix
    | VaultFileAdded MarkdownFile
    | VaultFileDeleted String
    | VaultFileRenamed ( String, String )
    | VaultFileUpdated MarkdownFile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ActiveStateUpdated isActiveView, _ ) ->
            ( mapSessionConfig (\c -> { c | isActiveView = isActiveView }) model
            , Cmd.none
            )

        ( AllMarkdownLoaded, _ ) ->
            ( mapSession Session.finishAdding model
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown <| Session.cards (toSession model)
                , InteropPorts.addHoverToCardEditButtons <| Session.cards (toSession model)
                ]
            )

        ( BadInputFromTypeScript, _ ) ->
            ( model, Cmd.none )

        ( BoardConfigsUpdated newConfigs, _ ) ->
            let
                newModel =
                    mapSession (Session.updateConfigs newConfigs) model
            in
            ( newModel
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown <| Session.cards (toSession newModel)
                , InteropPorts.addHoverToCardEditButtons <| Session.cards (toSession newModel)
                ]
            )

        ( FolderPathsReceived folderPaths, Settings subModel ) ->
            SettingsPage.update (SettingsPage.FolderPathsReceived folderPaths) subModel
                |> updateWith Settings GotSettingsPageMsg

        ( FolderPathsReceived folderPaths, _ ) ->
            ( model, Cmd.none )

        ( GotBoardPageMsg subMsg, Boards subModel ) ->
            BoardPage.update subMsg subModel
                |> updateWith Boards GotBoardPageMsg

        ( GotBoardPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotSettingsPageMsg subMsg, Settings subModel ) ->
            SettingsPage.update subMsg subModel
                |> updateWith Settings GotSettingsPageMsg

        ( GotSettingsPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( KeyDown keyValue, Settings subModel ) ->
            case ( keyValue, Session.isActiveView (toSession model) ) of
                ( Control "Escape", True ) ->
                    SettingsPage.update SettingsPage.ModalCloseClicked subModel
                        |> updateWith Settings GotSettingsPageMsg

                _ ->
                    ( model, Cmd.none )

        ( KeyDown _, _ ) ->
            ( model, Cmd.none )

        ( ReceiveTime ( zone, posix ), _ ) ->
            ( mapSessionConfig (\c -> { c | timeWithZone = { zone = zone, now = posix } }) model
            , Cmd.none
            )

        ( ShowBoard index, _ ) ->
            ( mapSessionConfig (\c -> { c | boardConfigs = SafeZipper.atIndex index c.boardConfigs }) model
            , Cmd.none
            )

        ( Tick time, _ ) ->
            ( mapSessionConfig (\c -> { c | timeWithZone = TimeWithZone.now time c.timeWithZone }) model
            , Cmd.none
            )

        ( VaultFileAdded markdownFile, _ ) ->
            let
                newTasks =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( mapSession (\s -> Session.addTaskList newTasks s) model
            , cmdForTaskRedraws newTasks (toSession model)
            )

        ( VaultFileDeleted filePath, _ ) ->
            ( mapSession (\s -> Session.deleteItemsFromFile filePath s) model
            , Cmd.none
            )

        ( VaultFileRenamed ( oldPath, newPath ), _ ) ->
            let
                updatedTaskItems =
                    rePathedTaskItems oldPath newPath (Session.taskList <| toSession model)
            in
            ( mapSession (\s -> Session.updateTaskItems oldPath updatedTaskItems s) model
            , cmdForTaskRedraws updatedTaskItems (toSession model)
            )

        ( VaultFileUpdated markdownFile, _ ) ->
            let
                updatedTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( mapSession (\s -> Session.updateTaskItems markdownFile.filePath updatedTaskItems s) model
            , cmdForTaskRedraws updatedTaskItems (toSession model)
            )


cmdForTaskRedraws : TaskList -> Session -> Cmd Msg
cmdForTaskRedraws newTasks session =
    let
        cards =
            newTasks
                |> Boards.init (Session.boardConfigs session)
                |> Boards.cards (Session.timeWithZone session)
    in
    if Session.taskListLoaded session then
        Cmd.batch
            [ InteropPorts.displayTaskMarkdown cards
            , InteropPorts.addHoverToCardEditButtons cards
            ]

    else
        Cmd.none


rePathedTaskItems : String -> String -> State TaskList -> TaskList
rePathedTaskItems oldPath newPath taskList =
    let
        rePathedItems : TaskList -> TaskList
        rePathedItems list =
            list
                |> TaskList.filter needsRename
                |> TaskList.map (TaskItem.updateFilePath newPath)

        needsRename : TaskItem -> Bool
        needsRename item =
            TaskItem.filePath item == oldPath
    in
    case taskList of
        State.Waiting ->
            TaskList.empty

        State.Loading currentList ->
            rePathedItems currentList

        State.Loaded currentList ->
            rePathedItems currentList


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg, Session.Msg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd, sessionMsg ) =
    ( case sessionMsg of
        Session.NoOp ->
            toModel subModel

        Session.SettingsClicked ->
            toModel subModel
                |> toSession
                |> SettingsPage.init
                |> Settings

        Session.SettingsClosed newConfigs ->
            toModel subModel
                |> toSession
                |> Session.mapConfig (\c -> { c | boardConfigs = newConfigs })
                |> Boards
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.onKeyDown (JD.map KeyDown keyDecoder)
        , InteropPorts.toElm
            |> Sub.map
                (\result ->
                    case result of
                        Ok toElm ->
                            case toElm of
                                InteropDefinitions.ActiveStateUpdated flag ->
                                    ActiveStateUpdated flag

                                InteropDefinitions.FileAdded markdownFile ->
                                    VaultFileAdded markdownFile

                                InteropDefinitions.FileDeleted filePath ->
                                    VaultFileDeleted filePath

                                InteropDefinitions.FileRenamed oldAndNewPath ->
                                    VaultFileRenamed oldAndNewPath

                                InteropDefinitions.FileUpdated markdownFile ->
                                    VaultFileUpdated markdownFile

                                InteropDefinitions.FolderPaths folderPaths ->
                                    FolderPathsReceived folderPaths

                                InteropDefinitions.AllMarkdownLoaded ->
                                    AllMarkdownLoaded

                                InteropDefinitions.SettingsUpdated newSettings ->
                                    BoardConfigsUpdated newSettings.boardConfigs

                                InteropDefinitions.ShowBoard index ->
                                    ShowBoard index

                        Err _ ->
                            BadInputFromTypeScript
                )
        ]


keyDecoder : JD.Decoder KeyValue
keyDecoder =
    JD.map toKeyValue (JD.field "key" JD.string)


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Boards session ->
            viewPage model
                GotBoardPageMsg
                GotSettingsPageMsg
                { content = BoardPage.view session
                , modal = Nothing
                }

        Settings settingsPageModel ->
            viewPage model
                GotBoardPageMsg
                GotSettingsPageMsg
                { content = BoardPage.view (SettingsPage.toSession settingsPageModel)
                , modal = Just <| SettingsPage.view settingsPageModel
                }

        StartupError session ->
            viewPage model
                GotBoardPageMsg
                GotSettingsPageMsg
                { content = BoardPage.view session
                , modal = Nothing
                }


viewPage : Model -> (contentMsg -> Msg) -> (modalMsg -> Msg) -> { content : Html contentMsg, modal : Maybe (Html modalMsg) } -> Html Msg
viewPage model toMsgContent toMsgModal pageView =
    let
        mappedView =
            { content = Html.map toMsgContent pageView.content
            , modal = Maybe.map (Html.map toMsgModal) pageView.modal
            }
    in
    Page.view (toSession model) mappedView
