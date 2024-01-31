module Worker exposing (main)

import Json.Decode as JD
import MarkdownFile exposing (MarkdownFile)
import TaskItem
import TaskList exposing (TaskList)
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
    | BadInputFromTypeScript
    | VaultFileAdded MarkdownFile
    | VaultFileDeleted String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AllMarkdownLoaded ->
            ( mapSession Session.finishAdding model, InteropPorts.allTasksLoaded )

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

        VaultFileDeleted filePath ->
            ( mapSession (\s -> Session.deleteItemsFromFile filePath s) model
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
                                InteropDefinitions.AllMarkdownLoaded ->
                                    AllMarkdownLoaded

                                InteropDefinitions.FileAdded markdownFile ->
                                    VaultFileAdded markdownFile

                                InteropDefinitions.FileDeleted filePath ->
                                    VaultFileDeleted filePath

                        Err _ ->
                            BadInputFromTypeScript
                )
        ]
