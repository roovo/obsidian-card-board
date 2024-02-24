module Main exposing (Model, Msg, main)

import BoardConfig
import Boards
import Browser
import Browser.Events as Browser
import Card exposing (Card)
import Date exposing (Date)
import DragAndDrop.DragData exposing (DragData)
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
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TextDirection exposing (TextDirection)
import Time exposing (Posix)
import TimeWithZone exposing (TimeWithZone)


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
                , InteropPorts.elmInitialized ""
                ]
            )

        Ok okFlags ->
            let
                session : Session
                session =
                    Session.fromFlags okFlags
            in
            ( Boards (BoardPage.init session)
                |> forceAddWhenNoBoards
            , Cmd.batch
                [ InteropPorts.updateSettings <| Session.settings session
                , InteropPorts.elmInitialized okFlags.uniqueId
                , Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
                ]
            )


forceAddWhenNoBoards : Model -> Model
forceAddWhenNoBoards model =
    case model of
        Boards _ ->
            if SafeZipper.length (Session.boardConfigs <| toSession model) == 0 then
                Settings (SettingsPage.init <| toSession model)

            else
                model

        _ ->
            model



-- MODEL


type Model
    = Boards BoardPage.Model
    | Settings SettingsPage.Model


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    case model of
        Boards boardPageModel ->
            Boards <| BoardPage.mapSession fn boardPageModel

        Settings settingsPageModel ->
            Settings <| SettingsPage.mapSession fn settingsPageModel


toSession : Model -> Session
toSession model =
    case model of
        Boards boardPageModel ->
            BoardPage.toSession boardPageModel

        Settings settingsPageModel ->
            SettingsPage.toSession settingsPageModel



-- UPDATE


type KeyValue
    = Character
    | Control String


type Msg
    = ActiveStateUpdated Bool
    | BadInputFromTypeScript
    | ConfigChanged TextDirection
    | EditCardDueDateRequested String
    | ElementDragged DragData
    | FilterCandidatesReceived (List Filter)
    | GotBoardPageMsg BoardPage.Msg
    | GotSettingsPageMsg SettingsPage.Msg
    | KeyDown KeyValue
    | ReceiveTime ( Time.Zone, Posix )
    | SettingsUpdated Settings
    | ShowBoard Int
    | Tick Posix
    | TaskItemsAdded TaskList
    | TaskItemsDeletedAndAdded ( List String, List TaskItem )
    | TaskItemsRefreshed TaskList
    | TaskItemsDeleted (List String)
    | TaskItemsUpdated (List ( String, TaskItem ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ActiveStateUpdated isActiveView, _ ) ->
            ( mapSession (Session.makeActiveView isActiveView) model
            , Cmd.none
            )

        ( BadInputFromTypeScript, _ ) ->
            ( model, Cmd.none )

        ( ConfigChanged textDirection, _ ) ->
            ( mapSession (Session.updateTextDirection textDirection) model, Cmd.none )

        ( EditCardDueDateRequested cardId, Boards subModel ) ->
            BoardPage.update (BoardPage.EditCardDueDateRequested cardId) subModel
                |> updateWith Boards GotBoardPageMsg

        ( EditCardDueDateRequested _, Settings _ ) ->
            ( model, Cmd.none )

        ( ElementDragged dragData, Boards subModel ) ->
            BoardPage.update (BoardPage.ElementDragged dragData) subModel
                |> updateWith Boards GotBoardPageMsg

        ( ElementDragged dragData, Settings subModel ) ->
            SettingsPage.update (SettingsPage.ElementDragged dragData) subModel
                |> updateWith Settings GotSettingsPageMsg

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
            ( mapSession (Session.timeWithZoneIs zone posix) model
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
            let
                cmd : Cmd Msg
                cmd =
                    if newDate == oldDate then
                        Cmd.none

                    else
                        cmdForDateChange newSession

                newDate : Int
                newDate =
                    newSession
                        |> Session.timeWithZone
                        |> TimeWithZone.toDate
                        |> Date.toRataDie

                newSession : Session
                newSession =
                    Session.timeIs time <| toSession model

                offsetPosix : Int -> TimeWithZone -> TimeWithZone
                offsetPosix secs timeWithZone =
                    let
                        foo : Posix -> Posix
                        foo posix =
                            posix
                                |> Time.posixToMillis
                                |> (\m -> m - (secs * 1000))
                                |> Time.millisToPosix
                    in
                    { timeWithZone | time = foo time }

                oldDate : Int
                oldDate =
                    toSession model
                        |> Session.timeWithZone
                        |> offsetPosix 1
                        |> TimeWithZone.toDate
                        |> Date.toRataDie
            in
            ( mapSession (always newSession) model
            , cmd
            )

        ( TaskItemsAdded taskList, _ ) ->
            ( mapSession (Session.addTaskList taskList) model
            , cmdForTaskRedraws taskList (toSession model)
            )

        ( TaskItemsDeleted taskItems, _ ) ->
            ( mapSession (Session.removeTaskItems taskItems) model
            , Cmd.none
            )

        ( TaskItemsDeletedAndAdded ( deleteIds, toAdd ), _ ) ->
            let
                addList : TaskList
                addList =
                    TaskList.fromList toAdd
            in
            ( model
                |> mapSession (Session.removeTaskItems deleteIds)
                |> mapSession (Session.addTaskList addList)
            , cmdForTaskRedraws addList (toSession model)
            )

        ( TaskItemsRefreshed taskList, _ ) ->
            ( mapSession (Session.replaceTaskList taskList) model
            , Cmd.none
            )

        ( TaskItemsUpdated updateDetails, _ ) ->
            let
                tasksToRedraw =
                    updateDetails
                        |> List.map Tuple.second
                        |> TaskList.fromList
            in
            ( mapSession (Session.replaceTaskItems updateDetails) model
            , cmdForTaskRedraws tasksToRedraw (toSession model)
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


cmdForDateChange : Session -> Cmd Msg
cmdForDateChange session =
    let
        today : Date
        today =
            Session.timeWithZone session
                |> TimeWithZone.toDate

        cards : List Card
        cards =
            Session.taskList session
                |> TaskList.filter TaskItem.isDated
                |> Boards.init (Session.uniqueId session) (Session.boardConfigs session)
                |> Boards.cards (Session.ignoreFileNameDates session) today
    in
    if List.isEmpty cards then
        Cmd.none

    else
        Cmd.batch
            [ InteropPorts.displayTaskMarkdown cards
            , InteropPorts.addHoverToCardEditButtons cards
            ]


cmdForTaskRedraws : TaskList -> Session -> Cmd Msg
cmdForTaskRedraws taskList session =
    let
        today : Date
        today =
            Session.timeWithZone session
                |> TimeWithZone.toDate

        cards : List Card
        cards =
            taskList
                |> Boards.init (Session.uniqueId session) (Session.boardConfigs session)
                |> Boards.cards (Session.ignoreFileNameDates session) today
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
                |> BoardPage.init
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

                                InteropDefinitions.EditCardDueDate cardId ->
                                    EditCardDueDateRequested cardId

                                InteropDefinitions.ElementDragged dragData ->
                                    ElementDragged dragData

                                InteropDefinitions.FilterCandidates filterCandidates ->
                                    FilterCandidatesReceived filterCandidates

                                InteropDefinitions.SettingsUpdated newSettings ->
                                    SettingsUpdated newSettings

                                InteropDefinitions.ShowBoard index ->
                                    ShowBoard index

                                InteropDefinitions.TaskItemsAdded taskItems ->
                                    TaskItemsAdded taskItems

                                InteropDefinitions.TaskItemsDeleted taskIds ->
                                    TaskItemsDeleted taskIds

                                InteropDefinitions.TaskItemsDeletedAndAdded ( toDelete, toAdd ) ->
                                    TaskItemsDeletedAndAdded ( toDelete, toAdd )

                                InteropDefinitions.TaskItemsRefreshed taskItems ->
                                    TaskItemsRefreshed taskItems

                                InteropDefinitions.TaskItemsUpdated updateDetails ->
                                    TaskItemsUpdated updateDetails

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
        Boards boardPageModel ->
            viewPage
                GotBoardPageMsg
                GotSettingsPageMsg
                { content = BoardPage.view boardPageModel
                , modal = Nothing
                }

        Settings settingsPageModel ->
            viewPage
                GotBoardPageMsg
                GotSettingsPageMsg
                { content =
                    BoardPage.view
                        (BoardPage.init <| SettingsPage.toSession settingsPageModel)
                , modal = Just <| SettingsPage.view settingsPageModel
                }


viewPage : (contentMsg -> Msg) -> (modalMsg -> Msg) -> { content : Html contentMsg, modal : Maybe (Html modalMsg) } -> Html Msg
viewPage toMsgContent toMsgModal pageView =
    Page.view
        { content = Html.map toMsgContent pageView.content
        , modal = Maybe.map (Html.map toMsgModal) pageView.modal
        }
