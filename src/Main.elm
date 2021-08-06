module Main exposing (main)

import Browser
import Date exposing (Date)
import FontAwesome.Attributes as FaAttributes
import FontAwesome.Icon as FaIcon
import FontAwesome.Regular as FaRegular
import FontAwesome.Styles as FaStyles
import FontAwesome.Svg as FaSvg
import Html exposing (Html)
import Html.Attributes exposing (checked, class, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import Parser
import Ports exposing (MarkdownFile)
import Task exposing (Task)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time


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
    , zone : Maybe Time.Zone
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
    ( { dailyNotesFolder = flags.folder
      , dailyNotesFormat = flags.format
      , today = Nothing
      , zone = Nothing
      , taskList = Loading
      }
    , Task.perform ReceiveTime <| Task.map2 Tuple.pair Time.here Time.now
    )



-- UPDATE


type Msg
    = ReceiveTime ( Time.Zone, Time.Posix )
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String
    | TaskItemToggled String
    | Tick Time.Posix
    | VaultFileAdded MarkdownFile
    | VaultFileDeleted String
    | VaultFileUpdated MarkdownFile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceiveTime ( zone, posix ), _ ) ->
            ( { model
                | zone = Just zone
                , today = Just (Date.fromPosix zone posix)
              }
            , Cmd.none
            )

        ( TaskItemDeleteClicked id, _ ) ->
            case model.taskList of
                Loaded taskList ->
                    case TaskList.taskFromId id taskList of
                        Just matchingItem ->
                            ( model
                            , Ports.deleteTodo
                                { filePath = TaskItem.filePath matchingItem
                                , lineNumber = TaskItem.lineNumber matchingItem
                                , originalText = TaskItem.originalText matchingItem
                                }
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ( TaskItemEditClicked id, _ ) ->
            case model.taskList of
                Loaded taskList ->
                    case TaskList.taskFromId id taskList of
                        Just matchingItem ->
                            ( model
                            , Ports.editTodo
                                { filePath = TaskItem.filePath matchingItem
                                , lineNumber = TaskItem.lineNumber matchingItem
                                , originalText = TaskItem.originalText matchingItem
                                }
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ( TaskItemToggled id, _ ) ->
            case model.taskList of
                Loaded taskList ->
                    case TaskList.taskFromId id taskList of
                        Just matchingItem ->
                            ( model
                            , Ports.rewriteTodo
                                { filePath = TaskItem.filePath matchingItem
                                , lineNumber = TaskItem.lineNumber matchingItem
                                , originalText = TaskItem.originalText matchingItem
                                , newText = matchingItem |> TaskItem.toggleCompletion model.today |> TaskItem.toString
                                }
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

        ( Tick time, _ ) ->
            ( { model
                | today = Just <| Date.fromPosix (Maybe.withDefault Time.utc model.zone) time
              }
            , Cmd.none
            )

        ( VaultFileAdded markdownFile, _ ) ->
            let
                newTaskItems =
                    Parser.run (TaskList.parser markdownFile.filePath markdownFile.fileDate) (markdownFile.fileContents ++ "\n")
                        |> Result.withDefault TaskList.empty
            in
            ( addTaskItems model newTaskItems
            , newTaskItems
                |> TaskList.toList
                |> List.map (\t -> { id = TaskItem.id t, titleMarkdown = TaskItem.title t })
                |> Ports.writeTaskTitles
            )

        ( VaultFileDeleted filePath, _ ) ->
            ( deleteItemsFromFile model filePath, Cmd.none )

        ( VaultFileUpdated markdownFile, _ ) ->
            ( Parser.run (TaskList.parser markdownFile.filePath markdownFile.fileDate) (markdownFile.fileContents ++ "\n")
                |> Result.withDefault TaskList.empty
                |> updateTaskItems model markdownFile.filePath
            , Cmd.none
            )


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
subscriptions model =
    Sub.batch
        [ Ports.fileAdded VaultFileAdded
        , Ports.fileUpdated VaultFileUpdated
        , Ports.fileDeleted VaultFileDeleted
        , Time.every (1000 * 60) Tick
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case ( model.taskList, model.today ) of
        ( Loaded taskList, Just today ) ->
            Html.div [ class "card-board" ]
                [ FaStyles.css
                , Html.div [ class "card-board-container" ]
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
        , Html.Keyed.ul [ class "card-board-column-list" ]
            (taskItems
                |> List.map (card title)
            )
        ]


card : String -> TaskItem -> ( String, Html Msg )
card columnTitle taskItem =
    let
        uniqueId =
            columnTitle ++ ":" ++ TaskItem.id taskItem
    in
    Html.li [ class "card-board-card", id uniqueId ]
        [ Html.div [ class "card-board-card-content-area" ]
            [ Html.div [ class "card-board-card-checkbox-area" ]
                [ Html.input
                    [ type_ "checkbox"
                    , onClick <| TaskItemToggled <| TaskItem.id taskItem
                    , checked <| TaskItem.isCompleted taskItem
                    ]
                    []
                ]
            , Html.div [ class "card-board-card-body" ]
                []
            ]
        , cardTagsView taskItem
            |> when (TaskItem.hasTags taskItem)
        , Html.div [ class "card-board-card-action-area" ]
            [ cardDueDate taskItem
                |> when (TaskItem.isDated taskItem)
            , cardActionButtons taskItem
            ]
        ]
        |> Tuple.pair uniqueId


cardTagsView : TaskItem -> Html Msg
cardTagsView taskItem =
    Html.div [ class "card-board-card-tag-area" ]
        (List.map cardTagView (TaskItem.tags taskItem))


cardTagView : String -> Html Msg
cardTagView tagText =
    Html.div [ class "card-board-card-tag" ]
        [ Html.span [ class "cm-hashtag-begin cm-hashtag" ]
            [ Html.text "#" ]
        , Html.span [ class "cm-hashtag cm-hashtag-end" ]
            [ Html.text tagText ]
        ]



-- <span class="cm-hashtag-begin cm-hashtag">#</span>
-- <span class="cm-hashtag cm-hashtag-end">people</span>
--
-- <span class="cm-list-1 cm-formatting cm-formatting-hashtag cm-hashtag-begin cm-hashtag cm-meta cm-tag-people">#</span>
-- <span class="cm-list-1 cm-hashtag cm-meta cm-hashtag-end cm-tag-people">people</span>


cardDueDate : TaskItem -> Html Msg
cardDueDate taskItem =
    Html.div [ class "card-board-card-action-area-due" ]
        [ Html.text ("Due: " ++ dueDateString taskItem)
        ]


dueDateString : TaskItem -> String
dueDateString taskItem =
    case TaskItem.due taskItem of
        Just dueDate ->
            Date.format "E, MMM ddd" dueDate

        Nothing ->
            "n/a"


cardActionButtons : TaskItem -> Html Msg
cardActionButtons taskItem =
    Html.div [ class "card-board-card-action-area-buttons" ]
        [ Html.button [ onClick <| TaskItemEditClicked <| TaskItem.id taskItem ]
            [ FaRegular.edit |> FaIcon.present |> FaIcon.styled [ FaAttributes.sm ] |> FaIcon.view ]
        , Html.button [ onClick <| TaskItemDeleteClicked <| TaskItem.id taskItem ]
            [ FaRegular.trashAlt |> FaIcon.present |> FaIcon.styled [ FaAttributes.sm ] |> FaIcon.view ]
        ]


empty : Html msg
empty =
    Html.text ""


when : Bool -> Html msg -> Html msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty
