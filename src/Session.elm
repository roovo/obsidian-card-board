module Session exposing
    ( Config
    , Msg(..)
    , Session
    , addTaskList
    , boardConfigs
    , cards
    , currentTaskList
    , default
    , deleteItemsFromFile
    , finishAdding
    , fromFlags
    , isActiveView
    , mapConfig
    , taskContainingId
    , taskFromId
    , taskList
    , taskListLoaded
    , timeWithZone
    , updateConfigs
    , updateTaskItems
    )

import BoardConfig exposing (BoardConfig)
import Boards
import Card exposing (Card)
import CardBoardSettings exposing (GlobalSettings)
import InteropDefinitions
import SafeZipper exposing (SafeZipper)
import State exposing (State)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Session
    = Session Config


type alias Config =
    { boardConfigs : SafeZipper BoardConfig
    , globalSettings : GlobalSettings
    , isActiveView : Bool
    , taskList : State TaskList
    , timeWithZone : TimeWithZone
    }



-- MSG


type Msg
    = NoOp
    | SettingsClicked
    | SettingsClosed


default : Session
default =
    Session
        { boardConfigs = SafeZipper.empty
        , globalSettings = CardBoardSettings.defaultGlobalSettings
        , isActiveView = False
        , taskList = State.Waiting
        , timeWithZone =
            { now = Time.millisToPosix 0
            , zone = Time.customZone 0 []
            }
        }


fromFlags : InteropDefinitions.Flags -> Session
fromFlags flags =
    Session
        { boardConfigs = SafeZipper.fromList <| CardBoardSettings.boardConfigs flags.settings
        , globalSettings = CardBoardSettings.globalSettings flags.settings
        , isActiveView = False
        , taskList = State.Waiting
        , timeWithZone =
            { now = Time.millisToPosix flags.now
            , zone = Time.customZone flags.zone []
            }
        }


mapConfig : (Config -> Config) -> Session -> Session
mapConfig fn (Session conf) =
    Session (fn conf)


boardConfigs : Session -> SafeZipper BoardConfig
boardConfigs (Session config) =
    config.boardConfigs


timeWithZone : Session -> TimeWithZone
timeWithZone (Session config) =
    config.timeWithZone


isActiveView : Session -> Bool
isActiveView (Session config) =
    config.isActiveView


taskList : Session -> State TaskList
taskList (Session config) =
    config.taskList



-- TASKLIST MANIPULATION


addTaskList : TaskList -> Session -> Session
addTaskList list ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            mapConfig (\c -> { c | taskList = State.Loading list }) session

        State.Loading currentList ->
            mapConfig (\c -> { c | taskList = State.Loading (TaskList.append currentList list) }) session

        State.Loaded currentList ->
            mapConfig (\c -> { c | taskList = State.Loaded (TaskList.append currentList list) }) session


deleteItemsFromFile : String -> Session -> Session
deleteItemsFromFile filePath ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            session

        State.Loading currentList ->
            mapConfig (\c -> { c | taskList = State.Loading (TaskList.removeForFile filePath currentList) }) session

        State.Loaded currentList ->
            mapConfig (\c -> { c | taskList = State.Loaded (TaskList.removeForFile filePath currentList) }) session


finishAdding : Session -> Session
finishAdding ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            mapConfig (\c -> { c | taskList = State.Loaded TaskList.empty }) session

        State.Loading list ->
            mapConfig (\c -> { c | taskList = State.Loaded list }) session

        State.Loaded _ ->
            session


updateTaskItems : String -> TaskList -> Session -> Session
updateTaskItems filePath updatedList ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            mapConfig (\c -> { c | taskList = State.Loading updatedList }) session

        State.Loading currentList ->
            mapConfig (\c -> { c | taskList = State.Loading (TaskList.replaceForFile filePath updatedList currentList) }) session

        State.Loaded currentList ->
            mapConfig (\c -> { c | taskList = State.Loaded (TaskList.replaceForFile filePath updatedList currentList) }) session



-- MISC


cards : Session -> List Card
cards ((Session config) as session) =
    session
        |> currentTaskList
        |> Boards.init config.boardConfigs
        |> Boards.cards config.timeWithZone


taskFromId : String -> Session -> Maybe TaskItem
taskFromId id (Session config) =
    case config.taskList of
        State.Loaded list ->
            TaskList.taskFromId id list

        State.Loading list ->
            TaskList.taskFromId id list

        _ ->
            Nothing


taskContainingId : String -> Session -> Maybe TaskItem
taskContainingId id (Session config) =
    case config.taskList of
        State.Loaded list ->
            TaskList.taskContainingId id list

        State.Loading list ->
            TaskList.taskContainingId id list

        _ ->
            Nothing


taskListLoaded : Session -> Bool
taskListLoaded (Session config) =
    State.hasLoaded config.taskList


updateConfigs : List BoardConfig -> Session -> Session
updateConfigs newConfigs ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            mapConfig (\c -> { c | boardConfigs = SafeZipper.fromList newConfigs }) session

        State.Loading _ ->
            mapConfig (\c -> { c | boardConfigs = SafeZipper.fromList newConfigs }) session

        State.Loaded _ ->
            let
                configs =
                    SafeZipper.fromList newConfigs
                        |> SafeZipper.atIndex newIndex

                newIndex =
                    config.boardConfigs
                        |> SafeZipper.currentIndex
                        |> Maybe.withDefault 0
            in
            mapConfig (\c -> { c | boardConfigs = configs }) session


currentTaskList : Session -> TaskList
currentTaskList (Session config) =
    case config.taskList of
        State.Waiting ->
            TaskList.empty

        State.Loading currentList ->
            currentList

        State.Loaded currentList ->
            currentList
