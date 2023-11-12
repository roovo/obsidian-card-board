module Session exposing
    ( DragStatus(..)
    , Msg(..)
    , Session
    , addTaskList
    , boardConfigs
    , cards
    , columnNames
    , dataviewTaskCompletion
    , default
    , deleteItemsFromFile
    , dragStatus
    , finishAdding
    , fromFlags
    , globalSettings
    , ignoreFileNameDates
    , isActiveView
    , makeActiveView
    , moveBoard
    , replaceTaskItems
    , settings
    , switchToBoardAt
    , taskContainingId
    , taskFromId
    , taskList
    , textDirection
    , timeIs
    , timeWIthZoneIs
    , timeWithZone
    , trackDraggable
    , uniqueId
    , updateColumnCollapse
    , updatePath
    , updateSettings
    , updateTextDirection
    )

import BoardConfig exposing (BoardConfig)
import Boards
import Card exposing (Card)
import ColumnNames exposing (ColumnNames)
import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import DragAndDrop.BeaconPosition exposing (BeaconPosition)
import DragAndDrop.DragData exposing (DragTracker)
import GlobalSettings exposing (GlobalSettings)
import InteropDefinitions
import SafeZipper exposing (SafeZipper)
import Settings exposing (Settings)
import State exposing (State)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import TextDirection exposing (TextDirection)
import Time
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type Session
    = Session Config


type alias Config =
    { dataviewTaskCompletion : DataviewTaskCompletion
    , dragStatus : DragStatus
    , isActiveView : Bool
    , settings : Settings
    , taskList : State TaskList
    , textDirection : TextDirection
    , timeWithZone : TimeWithZone
    , uniqueId : String
    }


type DragStatus
    = NotDragging
    | Dragging DragTracker


type Msg
    = NoOp
    | SettingsClicked
    | SettingsClosed Settings
    | TrackDraggable DragTracker



-- CREATE


default : Session
default =
    Session
        { dataviewTaskCompletion = DataviewTaskCompletion.default
        , dragStatus = NotDragging
        , isActiveView = False
        , settings = Settings.default
        , taskList = State.Waiting
        , textDirection = TextDirection.default
        , timeWithZone =
            { time = Time.millisToPosix 0
            , zone = Time.customZone 0 []
            }
        , uniqueId = "xXxXx"
        }


fromFlags : InteropDefinitions.Flags -> Session
fromFlags flags =
    Session
        { dataviewTaskCompletion = flags.dataviewTaskCompletion
        , dragStatus = NotDragging
        , isActiveView = False
        , settings = flags.settings
        , taskList = State.Waiting
        , textDirection = TextDirection.fromRtlFlag flags.rightToLeft
        , timeWithZone =
            { time = Time.millisToPosix flags.now
            , zone = Time.customZone flags.zone []
            }
        , uniqueId = flags.uniqueId
        }



-- UTILITIES


boardConfigs : Session -> SafeZipper BoardConfig
boardConfigs (Session config) =
    Settings.boardConfigs config.settings


cards : Session -> List Card
cards ((Session config) as session) =
    session
        |> taskList
        |> Boards.init config.uniqueId (columnNames session) (Settings.boardConfigs config.settings)
        |> Boards.cards (ignoreFileNameDates session) (TimeWithZone.toDate config.timeWithZone)


columnNames : Session -> ColumnNames
columnNames =
    .columnNames << globalSettings


dataviewTaskCompletion : Session -> DataviewTaskCompletion
dataviewTaskCompletion (Session config) =
    config.dataviewTaskCompletion


dragStatus : Session -> DragStatus
dragStatus (Session config) =
    config.dragStatus


globalSettings : Session -> GlobalSettings
globalSettings (Session config) =
    Settings.globalSettings config.settings


ignoreFileNameDates : Session -> Bool
ignoreFileNameDates =
    .ignoreFileNameDates << globalSettings


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


textDirection : Session -> TextDirection
textDirection (Session config) =
    config.textDirection


timeWithZone : Session -> TimeWithZone
timeWithZone (Session config) =
    config.timeWithZone


uniqueId : Session -> String
uniqueId (Session config) =
    config.uniqueId



-- TRANSFORM


makeActiveView : Bool -> Session -> Session
makeActiveView isActiveView_ (Session config) =
    Session { config | isActiveView = isActiveView_ }


moveBoard : String -> BeaconPosition -> Session -> Session
moveBoard draggedId beaconPosition (Session config) =
    Session { config | settings = Settings.moveBoard draggedId beaconPosition config.settings }


switchToBoardAt : Int -> Session -> Session
switchToBoardAt index (Session config) =
    Session { config | settings = Settings.switchToBoard index config.settings }


timeIs : Time.Posix -> Session -> Session
timeIs time (Session config) =
    Session { config | timeWithZone = TimeWithZone.time time config.timeWithZone }


timeWIthZoneIs : Time.Zone -> Time.Posix -> Session -> Session
timeWIthZoneIs zone time (Session config) =
    Session { config | timeWithZone = { zone = zone, time = time } }


trackDraggable : DragTracker -> Session -> Session
trackDraggable dragTracker (Session config) =
    Session { config | dragStatus = Dragging dragTracker }


updateSettings : Settings -> Session -> Session
updateSettings newSettings (Session config) =
    Session { config | settings = newSettings }


updateTextDirection : TextDirection -> Session -> Session
updateTextDirection newTextDirection (Session config) =
    Session { config | textDirection = newTextDirection }



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


updateColumnCollapse : Int -> Bool -> Session -> Session
updateColumnCollapse columnIndex isCollapsed (Session config) =
    Session
        { config
            | settings =
                Settings.updateCurrentBoard
                    (BoardConfig.collapseColumn columnIndex isCollapsed)
                    config.settings
        }


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


updateTaskListState : State TaskList -> Session -> Session
updateTaskListState taskListState (Session config) =
    Session { config | taskList = taskListState }
