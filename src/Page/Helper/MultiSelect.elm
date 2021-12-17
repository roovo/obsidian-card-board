module Page.Helper.Multiselect exposing
    ( Config
    , Model
    , Msg
    , SelectionItem
    , deleteHighlightedItem
    , init
    , receiveError
    , recieveItems
    , selectedItems
    , update
    , updateSelectedItems
    , view
    )

import AssocList as Dict exposing (Dict)
import Browser.Dom as Dom
import FeatherIcons
import Filter exposing (Filter)
import Fuzzy
import Html exposing (Html)
import Html.Attributes exposing (class, id, type_, value)
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
    , grouper : List (SelectionItem a) -> List ( String, List (SelectionItem a) )
    , selectedItemLabel : a -> String
    }


type alias Status a =
    { selectedItems : Dict String a
    , highlightedItem : String
    , searchTerm : String
    , page : Int
    , dropdownItems : List (SelectionItem a)
    , showDropDown : Bool
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
        , highlightedItem = ""
        , searchTerm = ""
        , page = 0
        , dropdownItems = []
        , showDropDown = False
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


highlightedItem : Model msg a -> String
highlightedItem =
    .highlightedItem << status


searchTerm : Model msg a -> String
searchTerm =
    .searchTerm << status


page : Model msg a -> Int
page =
    .page << status


dropdownItems : Model msg a -> List (SelectionItem a)
dropdownItems =
    .dropdownItems << status


showDropDown : Model msg a -> Bool
showDropDown =
    .showDropDown << status


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
                    , showDropDown = True
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


deleteHighlightedItem : Model msg a -> Model msg a
deleteHighlightedItem model =
    mapStatus
        (\s ->
            { s
                | selectedItems =
                    Dict.remove (highlightedItem model) s.selectedItems
                , highlightedItem = ""
            }
        )
        model


updateSelectedItems : Dict String a -> Model msg a -> Model msg a
updateSelectedItems selectedItems_ model =
    mapStatus (\s -> { s | selectedItems = selectedItems_ }) model



-- UPDATE


type Msg msg a
    = DelayedRequest String
    | DropdownScroll ScrollInfo
    | FocusGained (Result Dom.Error ())
    | FocusLost
    | ItemMouseDown
    | ItemSelected (SelectionItem a)
    | ChosenItemClicked String
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

        ChosenItemClicked label ->
            ( mapStatus (\s -> { s | highlightedItem = label }) model
            , Cmd.none
            )

        DelayedRequest delayedTerm ->
            if delayedTerm == searchTerm model then
                ( SettingItems
                    (config model)
                    { selectedItems = selectedItems model
                    , highlightedItem = highlightedItem model
                    , searchTerm = delayedTerm
                    , dropdownItems = dropdownItems model
                    , showDropDown = showDropDown model
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
                    |> mapStatus (\s -> { s | itemWasPressed = False, showDropDown = True })
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
            ( mapStatus (\s -> { s | searchTerm = newSearchTerm }) model
            , Cmd.none
            )

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
                        , highlightedItem = ""
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , showDropDown = False
                        , itemWasPressed = False
                        }

                AddingItems conf selectStatus ->
                    Ready conf
                        { selectedItems = selectStatus.selectedItems
                        , highlightedItem = ""
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , showDropDown = False
                        , itemWasPressed = False
                        }

                ReceivedItems conf selectStatus ->
                    ReceivedItems conf
                        { selectedItems = selectStatus.selectedItems
                        , highlightedItem = ""
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = selectStatus.dropdownItems
                        , showDropDown = False
                        , itemWasPressed = False
                        }

                ReceivedError conf selectStatus _ ->
                    Ready conf
                        { selectedItems = selectStatus.selectedItems
                        , highlightedItem = ""
                        , searchTerm = ""
                        , page = 0
                        , dropdownItems = []
                        , showDropDown = False
                        , itemWasPressed = False
                        }
    in
    ( newModel, Cmd.none )


primeRequest : String -> Model msg a -> ( Model msg a, Cmd msg )
primeRequest searchFor model =
    ( SettingItems
        (config model)
        { selectedItems = selectedItems model
        , highlightedItem = ""
        , searchTerm = searchFor
        , dropdownItems = dropdownItems model
        , showDropDown = showDropDown model
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
        , highlightedItem = highlightedItem model
        , searchTerm = searchTerm model
        , dropdownItems = dropdownItems model
        , showDropDown = showDropDown model
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
            (chosenItems (config model) (selectedItems model) (highlightedItem model) (tagger model)
                ++ [ input (tagger model) (searchTerm model) ]
            )
        , dropDownMenu model
        ]


chosenItems : Config msg a -> Dict String a -> String -> (Msg msg a -> msg) -> List (Html msg)
chosenItems conf selected highlighted msgTagger =
    selected
        |> Dict.foldr
            (\label filter acc ->
                chosenItem conf highlighted msgTagger filter label :: acc
            )
            []


chosenItem : Config msg a -> String -> (Msg msg a -> msg) -> a -> String -> Html msg
chosenItem conf highlighted msgTagger item itemText =
    let
        itemClass =
            if itemText == highlighted then
                "multiselect-item selected"

            else
                "multiselect-item"
    in
    Html.div
        [ class itemClass
        , Events.onMouseDown (ItemMouseDown |> msgTagger)
        , Events.onClick (ChosenItemClicked itemText |> msgTagger)
        ]
        [ Html.span [ class "multiselect-item-key" ]
            [ Html.text (conf.selectedItemLabel item) ]
        , Html.span [ class "multiselect-item-value" ]
            [ Html.text itemText ]
        ]


input : (Msg msg a -> msg) -> String -> Html msg
input msgTagger currentSearchTerm =
    Html.div []
        [ Html.input
            [ id "multiSelectInput"
            , class "multiselect-input"
            , type_ "text"
            , value currentSearchTerm
            , Events.onInput (SearchTermChanged >> msgTagger)
            , Events.onBlur (FocusLost |> msgTagger)
            ]
            []
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
    case ( selectStatus.showDropDown, selectStatus.dropdownItems ) of
        ( False, _ ) ->
            Html.text ""

        ( True, [] ) ->
            element

        ( True, _ ) ->
            selectStatus.dropdownItems
                |> fuzzyMatch selectStatus.searchTerm
                |> showSelectionSections conf
                |> wrapInDropDown msgTagger


fuzzyMatch : String -> List (SelectionItem a) -> List (SelectionItem a)
fuzzyMatch needle selectionItems =
    let
        score : SelectionItem a -> Maybe ( Int, SelectionItem a )
        score selectionItem =
            let
                result : Fuzzy.Result
                result =
                    Fuzzy.match [] [] (String.toLower needle) (String.toLower selectionItem.label)
            in
            if String.isEmpty needle then
                Just ( 0, selectionItem )

            else if result.score > 3000 then
                Nothing

            else
                Just ( result.score, selectionItem )
    in
    selectionItems
        |> List.filterMap score
        |> List.sortBy (\i -> String.toLower (Tuple.second i).label)
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


wrapInDropDown : (Msg msg a -> msg) -> List (Html msg) -> Html msg
wrapInDropDown msgTagger content =
    Html.div [ class "suggestion-container" ] content


showStatic : String -> List (Html msg)
showStatic content =
    [ Html.div []
        [ Html.text content ]
    ]


showSelectionSections : Config msg a -> List (SelectionItem a) -> List (Html msg)
showSelectionSections conf selections =
    selections
        |> conf.grouper
        |> List.map (showSelectionSection conf)


showSelectionSection : Config msg a -> ( String, List (SelectionItem a) ) -> Html msg
showSelectionSection conf ( title, selections ) =
    Html.div [ class "suggestion-section" ]
        [ Html.div [ class "suggestion-section-heading" ]
            [ Html.text title ]
        , Html.div []
            (showSelections conf selections)
        ]


showSelections : Config msg a -> List (SelectionItem a) -> List (Html msg)
showSelections conf selections =
    selections
        |> List.take 5
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
