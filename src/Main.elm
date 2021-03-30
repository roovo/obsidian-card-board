module Main exposing (main)

import Browser
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Parser
import Ports exposing (DataForElm)
import Task exposing (Task)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)


main : Program Flags Model Msg
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
    , today : Maybe Date
    , taskList : State TaskList
    }


type State a
    = Loading
    | Loaded a


type alias Flags =
    { folder : String
    , format : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        foo =
            Debug.log "flags" flags
    in
    ( { dailyNotesFolder = flags.folder
      , dailyNotesFormat = flags.format
      , today = Nothing
      , taskList = Loading
      }
    , Date.today |> Task.perform ReceiveDate
    )



-- UPDATE


type Msg
    = DataFromTypeScript DataForElm
    | LogError String
    | ReceiveDate Date


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( DataFromTypeScript dataForElm, _ ) ->
            case dataForElm of
                Ports.MarkdownToParse markdownFile ->
                    ( Parser.run (TaskList.parser markdownFile.filePath markdownFile.fileDate) (markdownFile.fileContents ++ "\n")
                        |> Result.withDefault TaskList.empty
                        |> addTaskItems model
                    , Cmd.none
                    )

                Ports.UpdatedMarkdownToParse markdownFile ->
                    ( Parser.run (TaskList.parser markdownFile.filePath markdownFile.fileDate) (markdownFile.fileContents ++ "\n")
                        |> Result.withDefault TaskList.empty
                        |> updateTaskItems model markdownFile.filePath
                    , Cmd.none
                    )

        ( LogError error, _ ) ->
            ( model, Cmd.none )

        ( ReceiveDate today, _ ) ->
            ( { model | today = Just today }
            , Cmd.none
            )


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
subscriptions model =
    Ports.receiveDataFromTypescript DataFromTypeScript LogError



-- VIEW


view : Model -> Html Msg
view model =
    case ( model.taskList, model.today ) of
        ( Loaded taskList, Just today ) ->
            Html.div [ class "card-board" ]
                [ Html.div [ class "card-board-container" ]
                    [ column
                        "Undated"
                        (TaskList.undatedItems taskList)
                    , column
                        "Today"
                        (TaskList.todaysItems today taskList)
                    , column
                        "Tomorrow"
                        (TaskList.tomorrowsItems today taskList)
                    , column
                        "Future"
                        (TaskList.futureItems today taskList)
                    , column
                        "Done"
                        (TaskList.completedItems taskList)
                    ]
                ]

        ( _, _ ) ->
            Html.text ""


column : String -> List TaskItem -> Html Msg
column title taskItems =
    Html.div [ class "card-board-column" ]
        [ Html.div [ class "card-board-column-header" ]
            [ Html.text title ]
        , Html.ul [ class "card-board-column-list" ]
            (taskItems
                |> List.map card
            )
        ]


card : TaskItem -> Html Msg
card taskItem =
    Html.li [ class "card-baord-card" ]
        [ Html.text <| TaskItem.title taskItem ]
