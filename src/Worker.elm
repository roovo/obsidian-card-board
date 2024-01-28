module Worker exposing (main)

import InteropDefinitions
import InteropPorts
import Json.Decode as JD
import MarkdownFile exposing (MarkdownFile)
import TaskItem
import TaskList exposing (TaskList)
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
            , InteropPorts.elmInitialized
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
    | BadInputFromTypeScript
    | VaultFileAdded MarkdownFile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AllMarkdownLoaded ->
            let
                foo =
                    model
                        |> toSession
                        |> Session.taskList
                        |> TaskList.tasks
                        |> List.map TaskItem.title
                        |> Debug.log "loaded tasks"
            in
            ( mapSession Session.finishAdding model, Cmd.none )

        BadInputFromTypeScript ->
            ( model, Cmd.none )

        VaultFileAdded markdownFile ->
            let
                newTasks : TaskList
                newTasks =
                    TaskList.fromMarkdown (Session.dataviewTaskCompletion <| toSession model) markdownFile

                newModel : Model
                newModel =
                    mapSession (\s -> Session.addTaskList newTasks s) model
            in
            ( newModel
            , Cmd.none
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
                                InteropDefinitions.FileAdded markdownFile ->
                                    VaultFileAdded markdownFile

                                InteropDefinitions.AllMarkdownLoaded ->
                                    AllMarkdownLoaded

                                _ ->
                                    BadInputFromTypeScript

                        Err _ ->
                            BadInputFromTypeScript
                )
        ]
