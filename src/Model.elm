module Model exposing
    ( Model
    , addTaskList
    , cards
    , default
    , deleteItemsFromFile
    , finishAdding
    , forceAddWhenNoBoards
    , fromFlags
    , mapBoardBeingAdded
    , mapBoardBeingEdited
    , mapSetingsEditState
    , taskContainingId
    , taskFromId
    , taskListLoaded
    , updateConfigBeingEdited
    , updateConfigs
    , updateTaskItems
    )

import BoardConfig exposing (BoardConfig)
import Boards
import Card exposing (Card)
import CardBoardSettings exposing (GlobalSettings)
import InteropDefinitions
import SafeZipper exposing (SafeZipper)
import SettingsEditState exposing (SettingsEditState)
import State exposing (State)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type alias Model =
    { boardConfigs : SafeZipper BoardConfig
    , globalSettings : GlobalSettings
    , isActiveView : Bool
    , settingsEditState : SettingsEditState
    , taskList : State TaskList
    , timeWithZone : TimeWithZone
    }


default : Model
default =
    { boardConfigs = SafeZipper.empty
    , globalSettings = CardBoardSettings.defaultGlobalSettings
    , isActiveView = False
    , settingsEditState = SettingsEditState.forNoConfig
    , taskList = State.Waiting
    , timeWithZone =
        { now = Time.millisToPosix 0
        , zone = Time.customZone 0 []
        }
    }


fromFlags : InteropDefinitions.Flags -> Model
fromFlags flags =
    { boardConfigs = SafeZipper.fromList <| CardBoardSettings.boardConfigs flags.settings
    , globalSettings = CardBoardSettings.globalSettings flags.settings
    , isActiveView = False
    , settingsEditState = SettingsEditState.default
    , taskList = State.Waiting
    , timeWithZone =
        { now = Time.millisToPosix flags.now
        , zone = Time.customZone flags.zone []
        }
    }
        |> forceAddWhenNoBoards



-- TASKLIST MANIPULATION


addTaskList : TaskList -> Model -> Model
addTaskList list model =
    case model.taskList of
        State.Waiting ->
            { model | taskList = State.Loading list }

        State.Loading currentList ->
            { model | taskList = State.Loading (TaskList.append currentList list) }

        State.Loaded currentList ->
            { model | taskList = State.Loaded (TaskList.append currentList list) }


deleteItemsFromFile : String -> Model -> Model
deleteItemsFromFile filePath model =
    case model.taskList of
        State.Waiting ->
            model

        State.Loading currentList ->
            { model | taskList = State.Loading (TaskList.removeForFile filePath currentList) }

        State.Loaded currentList ->
            { model | taskList = State.Loaded (TaskList.removeForFile filePath currentList) }


finishAdding : Model -> Model
finishAdding model =
    case model.taskList of
        State.Waiting ->
            { model | taskList = State.Loaded TaskList.empty }

        State.Loading list ->
            { model | taskList = State.Loaded list }

        State.Loaded _ ->
            model


updateTaskItems : String -> TaskList -> Model -> Model
updateTaskItems filePath updatedList model =
    case model.taskList of
        State.Waiting ->
            { model | taskList = State.Loading updatedList }

        State.Loading currentList ->
            { model | taskList = State.Loading (TaskList.replaceForFile filePath updatedList currentList) }

        State.Loaded currentList ->
            { model | taskList = State.Loaded (TaskList.replaceForFile filePath updatedList currentList) }



-- MISC


cards : Model -> List Card
cards model =
    model
        |> taskList
        |> Boards.init model.boardConfigs
        |> Boards.cards model.timeWithZone


forceAddWhenNoBoards : Model -> Model
forceAddWhenNoBoards model =
    if SafeZipper.length model.boardConfigs == 0 then
        { model | settingsEditState = SettingsEditState.forNoConfig }

    else
        model


mapBoardBeingAdded : (BoardConfig -> BoardConfig) -> Model -> Model
mapBoardBeingAdded fn model =
    { model
        | settingsEditState =
            SettingsEditState.mapBoardBeingAdded fn model.settingsEditState
    }


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> Model -> Model
mapBoardBeingEdited fn model =
    { model
        | settingsEditState =
            SettingsEditState.mapBoardBeingEdited fn model.settingsEditState
    }


mapSetingsEditState : (SettingsEditState -> SettingsEditState) -> Model -> Model
mapSetingsEditState fn model =
    { model | settingsEditState = fn model.settingsEditState }


taskFromId : String -> Model -> Maybe TaskItem
taskFromId id model =
    case model.taskList of
        State.Loaded list ->
            TaskList.taskFromId id list

        State.Loading list ->
            TaskList.taskFromId id list

        _ ->
            Nothing


taskContainingId : String -> Model -> Maybe TaskItem
taskContainingId id model =
    case model.taskList of
        State.Loaded list ->
            TaskList.taskContainingId id list

        State.Loading list ->
            TaskList.taskContainingId id list

        _ ->
            Nothing


taskListLoaded : Model -> Bool
taskListLoaded model =
    State.hasLoaded model.taskList


updateConfigBeingEdited : SettingsEditState -> Model -> Model
updateConfigBeingEdited newConfig model =
    { model | settingsEditState = newConfig }


updateConfigs : List BoardConfig -> Model -> Model
updateConfigs newConfigs model =
    case model.taskList of
        State.Waiting ->
            { model | boardConfigs = SafeZipper.fromList newConfigs }

        State.Loading _ ->
            { model | boardConfigs = SafeZipper.fromList newConfigs }

        State.Loaded _ ->
            let
                configs =
                    SafeZipper.fromList newConfigs
                        |> SafeZipper.atIndex newIndex

                newIndex =
                    model.boardConfigs
                        |> SafeZipper.currentIndex
                        |> Maybe.withDefault 0
            in
            { model | boardConfigs = configs }



-- HELPERS


taskList : Model -> TaskList
taskList model =
    case model.taskList of
        State.Waiting ->
            TaskList.empty

        State.Loading currentList ->
            currentList

        State.Loaded currentList ->
            currentList
