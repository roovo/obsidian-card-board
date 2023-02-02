module Main exposing (Model, Msg, main)

import BoardConfig
import Boards
import Browser
import Browser.Events as Browser
import Card exposing (Card)
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
import Settings exposing (Settings)
import Task
import TaskItem
import TaskList exposing (TaskList)
import TextDirection exposing (TextDirection)
import Time


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
                session : Session
                session =
                    Session.fromFlags okFlags
            in
            ( Boards session
                |> forceAddWhenNoBoards
            , Cmd.batch
                [ InteropPorts.updateSettings <| Session.settings session
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


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    case model of
        Boards session ->
            Boards <| fn session

        Settings settingsPageModel ->
            Settings <| SettingsPage.mapSession fn settingsPageModel


toSession : Model -> Session
toSession model =
    case model of
        Boards session ->
            session

        Settings settingsPageModel ->
            SettingsPage.toSession settingsPageModel



-- UPDATE


type KeyValue
    = Character
    | Control String


type Msg
    = ActiveStateUpdated Bool
    | AllMarkdownLoaded
    | BadInputFromTypeScript
    | ConfigChanged TextDirection
    | SettingsUpdated Settings
    | FilterCandidatesReceived (List Filter)
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
            ( mapSession (Session.makeActiveView isActiveView) model
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

        ( ConfigChanged textDirection, _ ) ->
            ( mapSession (Session.updateTextDirection textDirection) model, Cmd.none )

        ( FilterCandidatesReceived filterCandidates, Settings subModel ) ->
            SettingsPage.update (SettingsPage.FilterCandidatesReceived filterCandidates) subModel
                |> updateWith Settings GotSettingsPageMsg

        ( FilterCandidatesReceived _, _ ) ->
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
                ( Control "Backspace", True ) ->
                    SettingsPage.update SettingsPage.BackspacePressed subModel
                        |> updateWith Settings GotSettingsPageMsg

                ( Control "Escape", True ) ->
                    SettingsPage.update SettingsPage.ModalCloseClicked subModel
                        |> updateWith Settings GotSettingsPageMsg

                _ ->
                    ( model, Cmd.none )

        ( KeyDown _, _ ) ->
            ( model, Cmd.none )

        ( ReceiveTime ( zone, posix ), _ ) ->
            ( mapSession (Session.timeWIthZoneIs zone posix) model
            , Cmd.none
            )

        ( SettingsUpdated newSettings, _ ) ->
            let
                newIndex : Int
                newIndex =
                    model
                        |> toSession
                        |> Session.boardConfigs
                        |> SafeZipper.currentIndex
                        |> Maybe.withDefault 0

                newModel : Model
                newModel =
                    model
                        |> mapSession (Session.updateSettings newSettings)
                        |> mapSession (Session.switchToBoardAt newIndex)
            in
            ( newModel
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown <| Session.cards (toSession newModel)
                , InteropPorts.addHoverToCardEditButtons <| Session.cards (toSession newModel)
                ]
            )

        ( ShowBoard index, _ ) ->
            ( mapSession (Session.switchToBoardAt index) model
            , Cmd.none
            )

        ( Tick time, _ ) ->
            ( mapSession (Session.timeIs time) model
            , Cmd.none
            )

        ( VaultFileAdded markdownFile, _ ) ->
            let
                newTasks : TaskList
                newTasks =
                    TaskList.fromMarkdown (Session.dataviewTaskCompletion <| toSession model) markdownFile

                newModel : Model
                newModel =
                    mapSession (\s -> Session.addTaskList newTasks s) model
            in
            ( newModel
            , cmdForTaskRedraws markdownFile.filePath (toSession newModel)
            )

        ( VaultFileDeleted filePath, _ ) ->
            ( mapSession (\s -> Session.deleteItemsFromFile filePath s) model
            , Cmd.none
            )

        ( VaultFileRenamed ( oldPath, newPath ), _ ) ->
            let
                newModel : Model
                newModel =
                    mapSession (Session.updatePath oldPath newPath) model
            in
            ( newModel
            , Cmd.batch
                [ cmdForTaskRedraws newPath (toSession newModel)
                , cmdForFilterPathRename newPath (toSession newModel)
                ]
            )

        ( VaultFileUpdated markdownFile, _ ) ->
            let
                newTaskItems : TaskList
                newTaskItems =
                    TaskList.fromMarkdown (Session.dataviewTaskCompletion <| toSession model) markdownFile

                newModel : Model
                newModel =
                    mapSession (\s -> Session.replaceTaskItems markdownFile.filePath newTaskItems s) model
            in
            ( newModel
            , cmdForTaskRedraws markdownFile.filePath (toSession newModel)
            )


cmdForFilterPathRename : String -> Session -> Cmd msg
cmdForFilterPathRename newPath session =
    let
        anyUpdatedFilters : Bool
        anyUpdatedFilters =
            Session.boardConfigs session
                |> SafeZipper.toList
                |> List.concatMap BoardConfig.filters
                |> List.filter (\f -> Filter.filterType f == "Files" || Filter.filterType f == "Paths")
                |> List.any (\f -> Filter.value f == newPath)
    in
    if anyUpdatedFilters then
        InteropPorts.updateSettings <| Session.settings session

    else
        Cmd.none


cmdForTaskRedraws : String -> Session -> Cmd Msg
cmdForTaskRedraws newPath session =
    let
        cards : List Card
        cards =
            Session.taskList session
                |> TaskList.filter (\i -> TaskItem.filePath i == newPath)
                |> Boards.init (Session.uniqueId session) (Session.columnNames session) (Session.boardConfigs session)
                |> Boards.cards (Session.timeWithZone session)
    in
    if List.isEmpty cards then
        Cmd.none

    else
        Cmd.batch
            [ InteropPorts.displayTaskMarkdown cards
            , InteropPorts.addHoverToCardEditButtons cards
            ]


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

        Session.SettingsClosed newSettings ->
            toModel subModel
                |> toSession
                |> Session.updateSettings newSettings
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

                                InteropDefinitions.ConfigChanged textDirection ->
                                    ConfigChanged textDirection

                                InteropDefinitions.FileAdded markdownFile ->
                                    VaultFileAdded markdownFile

                                InteropDefinitions.FileDeleted filePath ->
                                    VaultFileDeleted filePath

                                InteropDefinitions.FileRenamed oldAndNewPath ->
                                    VaultFileRenamed oldAndNewPath

                                InteropDefinitions.FileUpdated markdownFile ->
                                    VaultFileUpdated markdownFile

                                InteropDefinitions.FilterCandidates filterCandidates ->
                                    FilterCandidatesReceived filterCandidates

                                InteropDefinitions.AllMarkdownLoaded ->
                                    AllMarkdownLoaded

                                InteropDefinitions.SettingsUpdated newSettings ->
                                    SettingsUpdated newSettings

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
        Just ( _, "" ) ->
            Character

        _ ->
            Control string



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Boards session ->
            viewPage
                GotBoardPageMsg
                GotSettingsPageMsg
                { content = BoardPage.view session
                , modal = Nothing
                }

        Settings settingsPageModel ->
            viewPage
                GotBoardPageMsg
                GotSettingsPageMsg
                { content = BoardPage.view (SettingsPage.toSession settingsPageModel)
                , modal = Just <| SettingsPage.view settingsPageModel
                }


viewPage : (contentMsg -> Msg) -> (modalMsg -> Msg) -> { content : Html contentMsg, modal : Maybe (Html modalMsg) } -> Html Msg
viewPage toMsgContent toMsgModal pageView =
    Page.view
        { content = Html.map toMsgContent pageView.content
        , modal = Maybe.map (Html.map toMsgModal) pageView.modal
        }
