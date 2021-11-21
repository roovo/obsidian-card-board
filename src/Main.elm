module Main exposing (main)

import BoardConfig exposing (BoardConfig)
import Browser
import Browser.Events as Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import InteropDefinitions
import InteropPorts
import Json.Decode as JD
import MarkdownFile exposing (MarkdownFile)
import Model exposing (Model)
import Page.Board as BoardPage
import Page.Settings as SettingsPage
import Panels
import SafeZipper
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
            ( Model.default, Cmd.none )

        Ok okFlags ->
            ( Model.fromFlags okFlags
            , Cmd.batch
                [ Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
                , InteropPorts.elmInitialized
                ]
            )



-- UPDATE


type KeyValue
    = Character Char
    | Control String


type Msg
    = ActiveStateUpdated Bool
    | AllMarkdownLoaded
    | BadInputFromTypeScript
    | BoardConfigsUpdated (List BoardConfig)
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
            ( { model | isActiveView = isActiveView }, Cmd.none )

        ( AllMarkdownLoaded, _ ) ->
            ( Model.finishAdding model
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown <| Model.cards model
                , InteropPorts.addHoverToCardEditButtons <| Model.cards model
                ]
            )

        ( BadInputFromTypeScript, _ ) ->
            ( model, Cmd.none )

        ( BoardConfigsUpdated newConfigs, _ ) ->
            let
                newModel =
                    Model.updateConfigs newConfigs model
            in
            ( newModel
            , Cmd.batch
                [ InteropPorts.displayTaskMarkdown <| Model.cards newModel
                , InteropPorts.addHoverToCardEditButtons <| Model.cards newModel
                ]
            )

        ( GotBoardPageMsg subMsg, _ ) ->
            BoardPage.update subMsg model
                |> updateWith GotBoardPageMsg

        ( GotSettingsPageMsg subMsg, _ ) ->
            SettingsPage.update subMsg model
                |> updateWith GotSettingsPageMsg

        ( KeyDown keyValue, _ ) ->
            case ( keyValue, model.isActiveView ) of
                ( Control "Escape", True ) ->
                    SettingsPage.update SettingsPage.ModalCloseClicked model
                        |> updateWith GotSettingsPageMsg

                _ ->
                    ( model, Cmd.none )

        ( ReceiveTime ( zone, posix ), _ ) ->
            ( { model | timeWithZone = { zone = zone, now = posix } }, Cmd.none )

        ( ShowBoard index, _ ) ->
            ( { model | boardConfigs = SafeZipper.atIndex index model.boardConfigs }, Cmd.none )

        ( Tick time, _ ) ->
            ( { model | timeWithZone = TimeWithZone.now time model.timeWithZone }
            , Cmd.none
            )

        ( VaultFileAdded markdownFile, _ ) ->
            let
                newTasks =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( Model.addTaskList newTasks model
            , cmdForTaskRedraws newTasks model
            )

        ( VaultFileDeleted filePath, _ ) ->
            ( Model.deleteItemsFromFile filePath model, Cmd.none )

        ( VaultFileRenamed ( oldPath, newPath ), _ ) ->
            let
                updatedTaskItems =
                    rePathedTaskItems oldPath newPath model.taskList
            in
            ( Model.updateTaskItems oldPath updatedTaskItems model
            , cmdForTaskRedraws updatedTaskItems model
            )

        ( VaultFileUpdated markdownFile, _ ) ->
            let
                updatedTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( Model.updateTaskItems markdownFile.filePath updatedTaskItems model
            , cmdForTaskRedraws updatedTaskItems model
            )


cmdForTaskRedraws : TaskList -> Model -> Cmd Msg
cmdForTaskRedraws newTasks model =
    let
        cards =
            newTasks
                |> Panels.init model.boardConfigs
                |> Panels.cards model.timeWithZone
    in
    if Model.taskListLoaded model then
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


updateWith : (subMsg -> Msg) -> ( Model, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toMsg ( model, subCmd ) =
    ( model, Cmd.map toMsg subCmd )



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
    case model.taskList of
        State.Loaded taskList ->
            Html.div [ class "card-board" ]
                [ Html.div [ class "card-board-container" ]
                    [ BoardPage.view model.timeWithZone model.boardConfigs taskList
                        |> Html.map GotBoardPageMsg
                    , SettingsPage.dialogs model.configBeingEdited
                        |> Html.map GotSettingsPageMsg
                    ]
                ]

        _ ->
            Html.text "Loading tasks...."
