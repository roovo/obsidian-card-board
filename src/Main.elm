module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class)
import Parser
import Ports exposing (DataForElm)
import TaskItem exposing (TaskItem)
import TaskItems


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Loading
    | Loaded (List TaskItem)


init : String -> ( Model, Cmd Msg )
init displayText =
    ( Loading
    , Cmd.none
    )



-- UPDATE


type Msg
    = DataFromTypeScript DataForElm
    | LogError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( DataFromTypeScript dataForElm, _ ) ->
            case dataForElm of
                Ports.MarkdownToParse markdownFile ->
                    ( Parser.run TaskItems.parser markdownFile.fileContents
                        |> Result.withDefault []
                        |> addToModel model
                    , Cmd.none
                    )

        ( LogError error, _ ) ->
            ( model, Cmd.none )


addToModel : Model -> List TaskItem -> Model
addToModel model taskItems =
    case model of
        Loading ->
            Loaded taskItems

        Loaded currentItems ->
            Loaded (currentItems ++ taskItems)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.receiveDataFromTypescript DataFromTypeScript LogError



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.text ""

        Loaded taskItems ->
            Html.div [ class "card-board" ]
                [ Html.div [ class "card-board-container" ]
                    [ column
                        "To Do"
                        (taskItems
                            |> List.filter (\t -> not <| TaskItem.isCompleted t)
                        )
                    , column
                        "Done"
                        (taskItems
                            |> List.filter (\t -> TaskItem.isCompleted t)
                        )
                    ]
                ]


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
