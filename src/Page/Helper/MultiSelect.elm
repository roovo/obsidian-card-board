module Page.Helper.Multiselect exposing
    ( Config
    , Model
    , Msg
    , SelectionItem
    , init
    , receiveError
    , recieveItems
    , selectedItems
    , update
    , view
    )

-- import Element exposing (Element)
-- import Element.Background as Background
-- import Element.Border as Border
-- import Element.Events as Events exposing (onClick)
-- import Element.Font as Font
-- import Element.Input as Input
-- import Page.Helper.Style as Style

import AssocList as Dict exposing (Dict)
import Browser.Dom as Dom
import FeatherIcons
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Process
import Task



-- MODEL


type Model msg a
    = Ready (Config msg a) (Status a)
    | SettingItems (Config msg a) (Status a)
    | AddingItems (Config msg a) (Status a)
    | ReceivedItems (Config msg a) (Status a)
    | ReceivedError (Config msg a) (Status a) String


type alias Config msg a =
    { delayMs : Float
    , tagger : Msg msg a -> msg
    , fetchMsg : Int -> String -> msg
    , notFoundText : String
    }


type alias Status a =
    { selectedItems : Dict String a
    , searchTerm : String
    , page : Int
    , dropdownItems : List (SelectionItem a)
    , itemWasPressed : Bool
    }


type alias SelectionItem a =
    { label : String
    , value : a
    }


init : Config msg a -> Dict String a -> Model msg a
init initialConfig selected =
    Ready initialConfig
        { selectedItems = selected
        , searchTerm = ""
        , page = 0
        , dropdownItems = []
        , itemWasPressed = False
        }



-- INFO


config : Model msg a -> Config msg a
config model =
    case model of
        Ready conf _ ->
            conf

        SettingItems conf _ ->
            conf

        AddingItems conf _ ->
            conf

        ReceivedItems conf _ ->
            conf

        ReceivedError conf _ _ ->
            conf


tagger : Model msg a -> (Msg msg a -> msg)
tagger =
    .tagger << config


status : Model msg a -> Status a
status model =
    case model of
        Ready _ selectStatus ->
            selectStatus

        SettingItems _ selectStatus ->
            selectStatus

        AddingItems _ selectStatus ->
            selectStatus

        ReceivedItems _ selectStatus ->
            selectStatus

        ReceivedError _ selectStatus _ ->
            selectStatus


selectedItems : Model msg a -> Dict String a
selectedItems =
    .selectedItems << status


searchTerm : Model msg a -> String
searchTerm =
    .searchTerm << status


page : Model msg a -> Int
page =
    .page << status


dropdownItems : Model msg a -> List (SelectionItem a)
dropdownItems =
    .dropdownItems << status


itemWasPressed : Model msg a -> Bool
itemWasPressed =
    .itemWasPressed << status



-- TRANSFORM


mapStatus : (Status a -> Status a) -> Model msg a -> Model msg a
mapStatus transform model =
    case model of
        Ready conf selectStatus ->
            Ready conf (transform selectStatus)

        SettingItems conf selectStatus ->
            SettingItems conf (transform selectStatus)

        AddingItems conf selectStatus ->
            AddingItems conf (transform selectStatus)

        ReceivedItems conf selectStatus ->
            ReceivedItems conf (transform selectStatus)

        ReceivedError conf selectStatus error ->
            ReceivedError conf (transform selectStatus) error


recieveItems : Model msg a -> List (SelectionItem a) -> Model msg a
recieveItems model items =
    case model of
        Ready _ _ ->
            model

        SettingItems conf selectStatus ->
            ReceivedItems
                conf
                { selectStatus
                    | dropdownItems = items
                    , itemWasPressed = False
                }

        AddingItems conf selectStatus ->
            ReceivedItems
                conf
                { selectStatus
                    | dropdownItems = selectStatus.dropdownItems ++ items
                    , itemWasPressed = False
                }

        ReceivedItems _ _ ->
            model

        ReceivedError _ _ _ ->
            model


receiveError : Model msg a -> String -> Model msg a
receiveError model error =
    ReceivedError (config model) (status model) error


addToSelected : SelectionItem a -> Model msg a -> Model msg a
addToSelected item model =
    mapStatus
        (\s ->
            { s
                | selectedItems =
                    Dict.insert item.label item.value s.selectedItems
            }
        )
        model
        |> mapStatus (\s -> { s | itemWasPressed = False })


deleteFromSelected : String -> Model msg a -> Model msg a
deleteFromSelected label model =
    mapStatus
        (\s ->
            { s
                | selectedItems =
                    Dict.remove label s.selectedItems
            }
        )
        model



-- UPDATE


type Msg msg a
    = DelayedRequest String
    | DropdownScroll ScrollInfo
    | FocusGained (Result Dom.Error ())
    | FocusLost
    | ItemMouseDown
    | ItemSelected (SelectionItem a)
    | ItemDeleteClicked String
    | SearchTermChanged String
    | SelectClicked
    | SendRequest


update : Msg msg a -> Model msg a -> ( Model msg a, Cmd msg )
update msg model =
    case msg of
        ItemSelected item ->
            ( addToSelected item model
            , setFocus <| tagger model
            )

        ItemDeleteClicked label ->
            ( deleteFromSelected label model
            , Cmd.none
            )

        DelayedRequest delayedTerm ->
            if delayedTerm == searchTerm model then
                ( SettingItems
                    (config model)
                    { selectedItems = selectedItems model
                    , searchTerm = delayedTerm
                    , dropdownItems = dropdownItems model
                    , itemWasPressed = False
                    , page = 0
                    }
                , performRequest <| tagger model
                )

            else
                ( model, Cmd.none )

        DropdownScroll { scrollHeight, scrollTop } ->
            if ifShouldFetchMore scrollTop scrollHeight model then
                fetchMore model

            else
                ( model, Cmd.none )

        FocusGained _ ->
            if itemWasPressed model && not (Dict.isEmpty (selectedItems model)) then
                ( model
                    |> mapStatus (\s -> { s | itemWasPressed = False })
                , Cmd.none
                )

            else if List.isEmpty (dropdownItems model) then
                primeRequest "" model

            else
                ( model
                    |> mapStatus (\s -> { s | itemWasPressed = False })
                , Cmd.none
                )

        FocusLost ->
            if itemWasPressed model then
                ( model, Cmd.none )

            else
                reset model

        ItemMouseDown ->
            ( mapStatus (\s -> { s | itemWasPressed = True }) model
            , Cmd.none
            )

        SearchTermChanged newSearchTerm ->
            primeRequest newSearchTerm model

        SelectClicked ->
            ( model
            , setFocus <| tagger model
            )

        SendRequest ->
            let
                fetchMsg =
                    (.fetchMsg << config) model
            in
            ( model
            , fetchMsg (page model) (searchTerm model)
                |> Task.succeed
                |> Task.perform identity
            )


setFocus : (Msg msg a -> msg) -> Cmd msg
setFocus msgTagger =
    Dom.focus "multiSelectInput"
        |> Task.attempt (FocusGained >> msgTagger)


reset : Model msg a -> ( Model msg a, Cmd msg )
reset model =
    let
        newModel =
            case model of
                Ready _ _ ->
                    model

                SettingItems conf selectStatus ->
                    Ready conf
                        { selectedItems = selectStatus.selectedItems
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , itemWasPressed = False
                        }

                AddingItems conf selectStatus ->
                    Ready conf
                        { selectedItems = selectStatus.selectedItems
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , itemWasPressed = False
                        }

                ReceivedItems conf selectStatus ->
                    Ready conf
                        { selectedItems = selectStatus.selectedItems
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , itemWasPressed = False
                        }

                ReceivedError conf selectStatus _ ->
                    Ready conf
                        { selectedItems = selectStatus.selectedItems
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , itemWasPressed = False
                        }
    in
    ( newModel, Cmd.none )


primeRequest : String -> Model msg a -> ( Model msg a, Cmd msg )
primeRequest searchFor model =
    ( SettingItems
        (config model)
        { selectedItems = selectedItems model
        , searchTerm = searchFor
        , dropdownItems = dropdownItems model
        , itemWasPressed = False
        , page = page model
        }
    , delayedSend ((.delayMs << config) model) (tagger model <| DelayedRequest searchFor)
    )


performRequest : (Msg msg a -> msg) -> Cmd msg
performRequest msgTagger =
    (SendRequest |> msgTagger)
        |> Task.succeed
        |> Task.perform identity


fetchMore : Model msg a -> ( Model msg a, Cmd msg )
fetchMore model =
    ( AddingItems
        (config model)
        { selectedItems = selectedItems model
        , searchTerm = searchTerm model
        , dropdownItems = dropdownItems model
        , itemWasPressed = False
        , page = page model + 1
        }
    , performRequest <| tagger model
    )


delayedSend : Float -> msg -> Cmd msg
delayedSend milli msg =
    Process.sleep milli
        |> Task.perform (\_ -> msg)



-- VIEW


view : Model msg a -> Html msg
view model =
    Html.div []
        [ Html.div
            [ class "multiselect-items"
            , Events.onClick (SelectClicked |> tagger model)
            ]
            (chosenItems (selectedItems model) (tagger model)
                ++ [ input (tagger model) (searchTerm model) ]
            )
        , dropDownMenu model
        ]


chosenItems : Dict String a -> (Msg msg a -> msg) -> List (Html msg)
chosenItems selected msgTagger =
    selected
        |> Dict.foldr
            (\label _ acc ->
                chosenItem label :: acc
             -- Html.div []
             --     [ Html.div
             --         [ Events.onClick (ItemDeleteClicked label |> msgTagger)
             --         , Events.onMouseDown (ItemMouseDown |> msgTagger)
             --         ]
             --         [ Html.text "x" ]
             --     , Html.div [] [ Html.text label ]
             --     ]
            )
            []


chosenItem : String -> Html msg
chosenItem itemText =
    Html.div [ class "multiselect-item" ]
        [ Html.span [ class "multiselect-item-key" ]
            [ Html.text "path" ]
        , Html.span [ class "multiselect-item-value" ]
            [ Html.text itemText ]
        ]


input : (Msg msg a -> msg) -> String -> Html msg
input msgTagger currentSearchTerm =
    Html.div []
        [ Html.input [ Events.onBlur (FocusLost |> msgTagger) ]
            []

        -- [ Element.htmlAttribute <|
        --     Html.Attributes.attribute "id" "multiSelectInput"
        -- , Element.width <| Element.minimum 100 Element.fill
        -- , Element.paddingXY 5 2
        -- , Border.width 0
        -- , inputBackground
        -- , Element.focused
        --     [ Border.shadow
        --         { offset = ( 0, 0 )
        --         , size = 0
        --         , blur = 0
        --         , color = Element.rgba 0 0 0 0.25
        --         }
        --     ]
        -- , Events.onLoseFocus (FocusLost |> msgTagger)
        -- ]
        -- { onChange = SearchTermChanged >> msgTagger
        -- , text = currentSearchTerm
        -- , placeholder = Nothing
        -- , label = Input.labelHidden "Location Multiselect"
        -- }
        ]


dropDownMenu : Model msg a -> Html msg
dropDownMenu model =
    case model of
        Ready conf selectStatus ->
            itemsOrDefault conf selectStatus (tagger model) (Html.div [] [])

        SettingItems conf selectStatus ->
            itemsOrDefault conf selectStatus (tagger model) (Html.div [] [])

        AddingItems conf selectStatus ->
            itemsOrDefault conf selectStatus (tagger model) (Html.div [] [])

        ReceivedItems conf selectStatus ->
            itemsOrDefault
                conf
                selectStatus
                (tagger model)
                (wrapInDropDown (tagger model) <| showStatic conf.notFoundText)

        ReceivedError conf selectStatus error ->
            itemsOrDefault
                conf
                selectStatus
                (tagger model)
                (wrapInDropDown (tagger model) <| showStatic error)


itemsOrDefault : Config msg a -> Status a -> (Msg msg a -> msg) -> Html msg -> Html msg
itemsOrDefault conf selectStatus msgTagger element =
    case selectStatus.dropdownItems of
        [] ->
            element

        _ ->
            showSelections conf selectStatus.dropdownItems
                |> wrapInDropDown msgTagger


wrapInDropDown : (Msg msg a -> msg) -> List (Html msg) -> Html msg
wrapInDropDown msgTagger content =
    Html.div
        [ class "suggestion-container" ]
        [ Html.div
            [-- , Element.htmlAttribute <|
             --     Html.Events.on "scroll" <|
             --         scrollPosition (DropdownScroll >> msgTagger)
            ]
            [ Html.div
                []
                content
            ]
        ]


showStatic : String -> List (Html msg)
showStatic content =
    [ Html.div []
        [ Html.text content ]
    ]


showSelections : Config msg a -> List (SelectionItem a) -> List (Html msg)
showSelections conf selections =
    selections
        |> List.map
            (\s ->
                Html.div
                    [ class "suggestion-item"
                    , Events.onMouseDown (ItemMouseDown |> conf.tagger)
                    , Events.onClick <| (ItemSelected s |> conf.tagger)
                    ]
                    [ Html.text s.label ]
            )



-- SCROLLING


type alias ScrollInfo =
    { scrollHeight : Int
    , scrollTop : Int
    }


scrollPosition : (ScrollInfo -> a) -> Decoder a
scrollPosition wrapper =
    Decode.map2 ScrollInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.map2 (+)
            (Decode.at [ "target", "scrollTop" ] Decode.int)
            (Decode.at [ "target", "clientHeight" ] Decode.int)
        )
        |> Decode.map wrapper


ifShouldFetchMore : Int -> Int -> Model msg a -> Bool
ifShouldFetchMore scrollTop scrollHeight model =
    case model of
        Ready _ _ ->
            False

        SettingItems _ _ ->
            False

        AddingItems _ _ ->
            False

        ReceivedItems _ selectStatus ->
            let
                scrollPageHeight =
                    toFloat scrollHeight / toFloat (selectStatus.page + 1)

                scrollPageTop =
                    toFloat scrollTop - toFloat scrollHeight + scrollPageHeight
            in
            scrollPageTop / scrollPageHeight > 0.7

        ReceivedError _ _ _ ->
            False
