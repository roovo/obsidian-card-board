module Session exposing
    ( Msg(..)
    , Session
    , TaskCompletionSettings
    , addTaskList
    , boardConfigs
    , cards
    , dataviewTaskCompletion
    , default
    , dragTracker
    , findCard
    , finishAdding
    , firstDayOfWeek
    , fromFlags
    , globalSettings
    , ignoreFileNameDates
    , isActiveView
    , isDragging
    , makeActiveView
    , moveBoard
    , moveColumn
    , moveDragable
    , removeTaskItems
    , replaceTaskItems
    , replaceTaskList
    , settings
    , stopTrackingDragable
    , switchToBoardAt
    , taskCompletionSettings
    , taskContainingId
    , taskFromId
    , taskList
    , textDirection
    , timeIs
    , timeWithZone
    , timeWithZoneIs
    , uniqueId
    , updateColumnCollapse
    , updatePath
    , updateSettings
    , updateTextDirection
    , waitForDrag
    )

import BoardConfig exposing (BoardConfig)
import Boards
import Card exposing (Card)
import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import Date
import DragAndDrop.BeaconPosition exposing (BeaconPosition)
import DragAndDrop.DragData exposing (DragData)
import DragAndDrop.DragTracker as DragTracker exposing (DragTracker)
import GlobalSettings exposing (GlobalSettings, TaskCompletionFormat)
import InteropDefinitions
import List.Extra as LE
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
    , dragTracker : DragTracker
    , firstDayOfWeek : Time.Weekday
    , isActiveView : Bool
    , settings : Settings
    , taskList : State TaskList
    , textDirection : TextDirection
    , timeWithZone : TimeWithZone
    , uniqueId : String
    }


type alias TaskCompletionSettings =
    { dataviewTaskCompletion : DataviewTaskCompletion
    , format : TaskCompletionFormat
    , inLocalTime : Bool
    , showUtcOffset : Bool
    }


type Msg
    = NoOp
    | SettingsClicked
    | SettingsClosed Settings



-- CREATE


default : Session
default =
    Session
        { dataviewTaskCompletion = DataviewTaskCompletion.default
        , dragTracker = DragTracker.init
        , firstDayOfWeek = Time.Mon
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
    let
        momentToWeekdayNumber : Int -> Int
        momentToWeekdayNumber dayNumber =
            if dayNumber == 0 then
                7

            else
                dayNumber
    in
    Session
        { dataviewTaskCompletion = flags.dataviewTaskCompletion
        , dragTracker = DragTracker.init
        , firstDayOfWeek = Date.numberToWeekday <| momentToWeekdayNumber flags.firstDayOfWeek
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



-- INFO


boardConfigs : Session -> SafeZipper BoardConfig
boardConfigs (Session config) =
    Settings.boardConfigs config.settings


cards : Session -> List Card
cards ((Session config) as session) =
    session
        |> taskList
        |> Boards.init config.uniqueId (Settings.boardConfigs config.settings)
        |> Boards.cards (ignoreFileNameDates session) (TimeWithZone.toDate config.timeWithZone)


dataviewTaskCompletion : Session -> DataviewTaskCompletion
dataviewTaskCompletion (Session config) =
    config.dataviewTaskCompletion


findCard : String -> Session -> Maybe Card
findCard cardId session =
    cards session
        |> LE.find (\c -> Card.id c == cardId)


dragTracker : Session -> DragTracker
dragTracker (Session config) =
    config.dragTracker


firstDayOfWeek : Session -> Date.Weekday
firstDayOfWeek ((Session config) as session) =
    case globalSettings session |> .firstDayOfWeek of
        GlobalSettings.FromLocale ->
            config.firstDayOfWeek

        GlobalSettings.SpecificWeekday weekday ->
            weekday


globalSettings : Session -> GlobalSettings
globalSettings (Session config) =
    Settings.globalSettings config.settings


ignoreFileNameDates : Session -> Bool
ignoreFileNameDates =
    .ignoreFileNameDates << globalSettings


isActiveView : Session -> Bool
isActiveView (Session config) =
    config.isActiveView


isDragging : Session -> Bool
isDragging (Session config) =
    DragTracker.isDragging config.dragTracker


settings : Session -> Settings
settings (Session config) =
    config.settings


taskCompletionSettings : Session -> TaskCompletionSettings
taskCompletionSettings session =
    let
        globalCompletionSettings : GlobalSettings.TaskCompletionSettings
        globalCompletionSettings =
            globalSettings session
                |> GlobalSettings.taskCompletionSettings
    in
    { dataviewTaskCompletion = dataviewTaskCompletion session
    , format = globalCompletionSettings.format
    , inLocalTime = globalCompletionSettings.inLocalTime
    , showUtcOffset = globalCompletionSettings.showUtcOffset
    }


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


moveColumn : String -> BeaconPosition -> Session -> Session
moveColumn draggedId beaconPosition (Session config) =
    Session { config | settings = Settings.moveColumn draggedId beaconPosition config.settings }


moveDragable : DragData -> Session -> Session
moveDragable dragData (Session config) =
    Session { config | dragTracker = DragTracker.moveDragable dragData config.dragTracker }


stopTrackingDragable : Session -> Session
stopTrackingDragable (Session config) =
    Session { config | dragTracker = DragTracker.stopTracking }


switchToBoardAt : Int -> Session -> Session
switchToBoardAt index (Session config) =
    Session { config | settings = Settings.switchToBoard index config.settings }


timeIs : Time.Posix -> Session -> Session
timeIs time (Session config) =
    Session { config | timeWithZone = TimeWithZone.updateTime time config.timeWithZone }


timeWithZoneIs : Time.Zone -> Time.Posix -> Session -> Session
timeWithZoneIs zone time (Session config) =
    Session { config | timeWithZone = { zone = zone, time = time } }


updateColumnCollapse : Int -> Bool -> Session -> Session
updateColumnCollapse columnIndex isCollapsed (Session config) =
    Session
        { config
            | settings =
                Settings.updateCurrentBoard
                    (BoardConfig.collapseColumn columnIndex isCollapsed)
                    config.settings
        }


updateSettings : Settings -> Session -> Session
updateSettings newSettings (Session config) =
    Session { config | settings = newSettings }


updateTextDirection : TextDirection -> Session -> Session
updateTextDirection newTextDirection (Session config) =
    Session { config | textDirection = newTextDirection }


waitForDrag : DragTracker.ClientData -> Session -> Session
waitForDrag clientData (Session config) =
    Session { config | dragTracker = DragTracker.waitForDrag clientData }



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


finishAdding : Session -> Session
finishAdding ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            updateTaskListState (State.Loaded TaskList.empty) session

        State.Loading list ->
            updateTaskListState (State.Loaded list) session

        State.Loaded _ ->
            session


removeTaskItems : List String -> Session -> Session
removeTaskItems taskIds ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            session

        State.Loading currentList ->
            updateTaskListState (State.Loading (TaskList.filter (\i -> not <| List.member (TaskItem.id i) taskIds) currentList)) session

        State.Loaded currentList ->
            updateTaskListState (State.Loaded (TaskList.filter (\i -> not <| List.member (TaskItem.id i) taskIds) currentList)) session


replaceTaskItems : List ( String, TaskItem ) -> Session -> Session
replaceTaskItems updateDetails ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            session

        State.Loading currentList ->
            updateTaskListState (State.Loading (TaskList.replaceTaskItems updateDetails currentList)) session

        State.Loaded currentList ->
            updateTaskListState (State.Loading (TaskList.replaceTaskItems updateDetails currentList)) session


replaceTaskList : TaskList -> Session -> Session
replaceTaskList newList ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            updateTaskListState (State.Loaded newList) session

        State.Loading _ ->
            updateTaskListState (State.Loaded newList) session

        State.Loaded _ ->
            updateTaskListState (State.Loaded newList) session


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
