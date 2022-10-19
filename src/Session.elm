module Session exposing
    ( Config
    , Msg(..)
    , Session
    , addTaskList
    , boardConfigs
    , cards
    , default
    , deleteItemsFromFile
    , finishAdding
    , fromFlags
    , globalSettings
    , isActiveView
    , makeActiveView
    , replaceTaskItems
    , settings
    , switchToBoardAt
    , taskContainingId
    , taskFromId
    , taskList
    , timeIs
    , timeWIthZoneIs
    , timeWithZone
    , updateConfigs
    , updateGlobalSettings
    , updatePath
    )

import BoardConfig exposing (BoardConfig)
import Boards
import Card exposing (Card)
import Filter
import GlobalSettings exposing (GlobalSettings)
import InteropDefinitions
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)
import State exposing (State)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Session
    = Session Config


type alias Config =
    { settings : Settings
    , isActiveView : Bool
    , taskList : State TaskList
    , timeWithZone : TimeWithZone
    }


type Msg
    = NoOp
    | SettingsClicked
    | SettingsClosed (SafeZipper BoardConfig)



-- CREATE


default : Session
default =
    Session
        { settings = Settings.default
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
        { settings = flags.settings
        , isActiveView = False
        , taskList = State.Waiting
        , timeWithZone =
            { now = Time.millisToPosix flags.now
            , zone = Time.customZone flags.zone []
            }
        }



-- UTILITIES


boardConfigs : Session -> SafeZipper BoardConfig
boardConfigs (Session config) =
    Settings.boardConfigs config.settings


cards : Session -> List Card
cards ((Session config) as session) =
    session
        |> taskList
        |> Boards.init (Settings.boardConfigs config.settings)
        |> Boards.cards config.timeWithZone


globalSettings : Session -> GlobalSettings
globalSettings (Session config) =
    Settings.globalSettings config.settings


isActiveView : Session -> Bool
isActiveView (Session config) =
    config.isActiveView


settings : Session -> Settings
settings (Session config) =
    config.settings


taskContainingId : String -> Session -> Maybe TaskItem
taskContainingId id (Session config) =
    case config.taskList of
        State.Loaded list ->
            TaskList.taskContainingId id list

        State.Loading list ->
            TaskList.taskContainingId id list

        _ ->
            Nothing


taskFromId : String -> Session -> Maybe TaskItem
taskFromId id (Session config) =
    case config.taskList of
        State.Loaded list ->
            TaskList.taskFromId id list

        State.Loading list ->
            TaskList.taskFromId id list

        _ ->
            Nothing


taskList : Session -> TaskList
taskList (Session config) =
    case config.taskList of
        State.Waiting ->
            TaskList.empty

        State.Loading currentList ->
            currentList

        State.Loaded currentList ->
            currentList


timeWithZone : Session -> TimeWithZone
timeWithZone (Session config) =
    config.timeWithZone



-- TRANSFORM


makeActiveView : Bool -> Session -> Session
makeActiveView isActiveView_ (Session config) =
    Session { config | isActiveView = isActiveView_ }


switchToBoardAt : Int -> Session -> Session
switchToBoardAt index (Session config) =
    Session { config | settings = Settings.switchToBoard index config.settings }


timeIs : Time.Posix -> Session -> Session
timeIs time (Session config) =
    Session { config | timeWithZone = TimeWithZone.now time config.timeWithZone }


timeWIthZoneIs : Time.Zone -> Time.Posix -> Session -> Session
timeWIthZoneIs zone time (Session config) =
    Session { config | timeWithZone = { zone = zone, now = time } }


updateConfigs : SafeZipper BoardConfig -> Session -> Session
updateConfigs newConfigs (Session config) =
    case config.taskList of
        State.Waiting ->
            Session { config | settings = Settings.updateBoardConfigs newConfigs config.settings }

        State.Loading _ ->
            Session { config | settings = Settings.updateBoardConfigs newConfigs config.settings }

        State.Loaded _ ->
            Session { config | settings = Settings.loadNewBoardConfigs newConfigs config.settings }


updateGlobalSettings : GlobalSettings -> Session -> Session
updateGlobalSettings newSettings (Session config) =
    Session { config | settings = Settings.updateGlobalSettings newSettings config.settings }



-- TASKLIST MANIPULATION


addTaskList : TaskList -> Session -> Session
addTaskList list ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            updateTaskListState (State.Loading list) session

        State.Loading currentList ->
            updateTaskListState (State.Loading (TaskList.append currentList list)) session

        State.Loaded currentList ->
            updateTaskListState (State.Loaded (TaskList.append currentList list)) session


deleteItemsFromFile : String -> Session -> Session
deleteItemsFromFile filePath ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            session

        State.Loading currentList ->
            updateTaskListState (State.Loading (TaskList.removeForFile filePath currentList)) session

        State.Loaded currentList ->
            updateTaskListState (State.Loaded (TaskList.removeForFile filePath currentList)) session


finishAdding : Session -> Session
finishAdding ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            updateTaskListState (State.Loaded TaskList.empty) session

        State.Loading list ->
            updateTaskListState (State.Loaded list) session

        State.Loaded _ ->
            session


replaceTaskItems : String -> TaskList -> Session -> Session
replaceTaskItems filePath updatedList ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            session

        State.Loading currentList ->
            updateTaskListState (State.Loading (TaskList.replaceForFile filePath updatedList currentList)) session

        State.Loaded currentList ->
            updateTaskListState (State.Loaded (TaskList.replaceForFile filePath updatedList currentList)) session


updatePath : String -> String -> Session -> Session
updatePath oldPath newPath (Session config) =
    let
        updateTaskListPaths : TaskList -> TaskList
        updateTaskListPaths tasks =
            TaskList.map (TaskItem.updateFilePath oldPath newPath) tasks
    in
    Session
        { config
            | settings = Settings.updatePath oldPath newPath config.settings
            , taskList = State.map updateTaskListPaths config.taskList
        }



-- PRIVATE


mapConfig : (Config -> Config) -> Session -> Session
mapConfig fn (Session conf) =
    Session (fn conf)


updateTaskListState : State TaskList -> Session -> Session
updateTaskListState taskListState (Session config) =
    Session { config | taskList = taskListState }
