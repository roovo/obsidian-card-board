module Worker exposing (main)

import Browser
import Card exposing (Card)
import CardBoard
import Date exposing (Date)
import FeatherIcons
import InteropDefinitions
import InteropPorts
import Json.Decode as JD
import MarkdownFile exposing (MarkdownFile)
import Task
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time


main : Program JD.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


type alias Model =
    { dailyNotesFolder : String
    , dailyNotesFormat : String
    , taskList : State TaskList
    }


type State a
    = Loading
    | Loaded a


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    case flags |> InteropPorts.decodeFlags of
        Err error ->
            Debug.todo <| Debug.toString error

        Ok okFlags ->
            ( { dailyNotesFolder = okFlags.folder
              , dailyNotesFormat = okFlags.format
              , taskList = Loading
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = BadInputFromTypeScript
    | VaultFileAdded MarkdownFile
    | VaultFileDeleted String
    | VaultFileUpdated MarkdownFile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( BadInputFromTypeScript, _ ) ->
            ( model, Cmd.none )

        ( VaultFileAdded markdownFile, _ ) ->
            let
                _ =
                    Debug.log "file" markdownFile.filePath

                newTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( addTaskItems model newTaskItems, Cmd.none )

        ( VaultFileDeleted filePath, _ ) ->
            ( deleteItemsFromFile model filePath, Cmd.none )

        ( VaultFileUpdated markdownFile, _ ) ->
            let
                updatedTaskItems =
                    TaskList.fromMarkdown markdownFile.filePath markdownFile.fileDate markdownFile.fileContents
            in
            ( updateTaskItems model markdownFile.filePath updatedTaskItems, Cmd.none )


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
    InteropPorts.toElm
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
