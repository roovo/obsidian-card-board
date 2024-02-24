module Worker exposing (main)

import Json.Decode as JD
import List.Extra as LE
import MarkdownFile exposing (MarkdownFile)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList, TaskListDiff)
import Worker.InteropDefinitions as InteropDefinitions
import Worker.InteropPorts as InteropPorts
import Worker.Session as Session exposing (Session)


main : Program JD.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = FlagsError Session
    | Yeah Session


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    case flags |> InteropPorts.decodeFlags of
        Err _ ->
            ( FlagsError Session.default, Cmd.none )

        Ok okFlags ->
            let
                session : Session
                session =
                    Session.fromFlags okFlags
            in
            ( Yeah session
            , Cmd.none
            )


toSession : Model -> Session
toSession model =
    case model of
        FlagsError session ->
            session

        Yeah session ->
            session


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    case model of
        FlagsError session ->
            FlagsError <| fn session

        Yeah session ->
            Yeah <| fn session



-- UPDATE


type Msg
    = AllMarkdownLoaded
    | ViewInitialized
    | BadInputFromTypeScript
    | VaultFileAdded MarkdownFile
    | VaultFileDeleted String
    | VaultFileModified MarkdownFile
    | VaultFileRenamed ( String, String )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AllMarkdownLoaded ->
            ( mapSession Session.finishAdding model, InteropPorts.allTasksLoaded )

        ViewInitialized ->
            ( model, InteropPorts.allTaskItems <| Session.taskList <| toSession model )

        BadInputFromTypeScript ->
            ( model, Cmd.none )

        VaultFileAdded markdownFile ->
            let
                newTasks : TaskList
                newTasks =
                    TaskList.fromMarkdown (Session.dataviewTaskCompletion <| toSession model) markdownFile
            in
            ( mapSession (Session.addTaskList newTasks) model
            , InteropPorts.tasksAdded newTasks
            )

        VaultFileDeleted filePath ->
            let
                ( toDelete, remaining ) =
                    toSession model
                        |> Session.taskList
                        |> TaskList.topLevelTasks
                        |> List.partition (TaskItem.isFromFile filePath)
            in
            ( mapSession (Session.replaceTaskList <| TaskList.fromList remaining) model
            , InteropPorts.tasksDeleted toDelete
            )

        VaultFileModified markdownFile ->
            let
                deleteIds : List String
                deleteIds =
                    List.map TaskItem.id taskListDiff.toDelete

                session : Session
                session =
                    toSession model

                taskListDiff : TaskListDiff
                taskListDiff =
                    TaskList.markdownDiffs
                        (Session.dataviewTaskCompletion session)
                        markdownFile
                        (Session.taskList session)
            in
            ( model
                |> mapSession (Session.removeTaskItems deleteIds)
                |> mapSession (Session.addTaskList <| TaskList.fromList taskListDiff.toAdd)
            , InteropPorts.tasksDeletedAndAdded taskListDiff.toDelete taskListDiff.toAdd
            )

        VaultFileRenamed ( oldPath, newPath ) ->
            let
                originalIds : List String
                originalIds =
                    List.map TaskItem.id toUpdate

                updatedTaskItems : List TaskItem
                updatedTaskItems =
                    List.map (TaskItem.updateFilePath oldPath newPath) toUpdate

                ( toUpdate, remaining ) =
                    toSession model
                        |> Session.taskList
                        |> TaskList.topLevelTasks
                        |> List.partition (TaskItem.isFromFile oldPath)
            in
            ( mapSession (Session.replaceTaskList <| TaskList.fromList (remaining ++ updatedTaskItems)) model
            , InteropPorts.tasksUpdated (LE.zip originalIds updatedTaskItems)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ InteropPorts.toElm
            |> Sub.map
                (\result ->
                    case result of
                        Ok toElm ->
                            case toElm of
                                InteropDefinitions.AllMarkdownLoaded ->
                                    AllMarkdownLoaded

                                InteropDefinitions.ViewInitialized ->
                                    ViewInitialized

                                InteropDefinitions.FileAdded markdownFile ->
                                    VaultFileAdded markdownFile

                                InteropDefinitions.FileDeleted filePath ->
                                    VaultFileDeleted filePath

                                InteropDefinitions.FileModified markdownFile ->
                                    VaultFileModified markdownFile

                                InteropDefinitions.FileRenamed oldAndNewPath ->
                                    VaultFileRenamed oldAndNewPath

                        Err _ ->
                            BadInputFromTypeScript
                )
        ]
