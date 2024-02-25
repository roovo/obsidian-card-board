module Worker.Session exposing
    ( Session
    , addTaskList
    , dataviewTaskCompletion
    , default
    , finishAdding
    , fromFlags
    , removeTaskItems
    , replaceTaskList
    , taskList
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import State exposing (State)
import TaskItem
import TaskList exposing (TaskList)
import Worker.InteropDefinitions as InteropDefinitions



-- TYPES


type Session
    = Session Model


type alias Model =
    { dataviewTaskCompletion : DataviewTaskCompletion
    , taskList : State TaskList
    }



-- CREATE


default : Session
default =
    Session
        { dataviewTaskCompletion = DataviewTaskCompletion.default
        , taskList = State.Waiting
        }


fromFlags : InteropDefinitions.Flags -> Session
fromFlags flags =
    Session
        { dataviewTaskCompletion = flags.dataviewTaskCompletion
        , taskList = State.Waiting
        }



-- INFO


dataviewTaskCompletion : Session -> DataviewTaskCompletion
dataviewTaskCompletion (Session model) =
    model.dataviewTaskCompletion


taskList : Session -> TaskList
taskList (Session model) =
    case model.taskList of
        State.Waiting ->
            TaskList.empty

        State.Loading currentList ->
            currentList

        State.Loaded currentList ->
            currentList



-- TASKLIST MANIPULATION


addTaskList : TaskList -> Session -> Session
addTaskList list ((Session model) as session) =
    case model.taskList of
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


replaceTaskList : TaskList -> Session -> Session
replaceTaskList newList ((Session config) as session) =
    case config.taskList of
        State.Waiting ->
            session

        State.Loading _ ->
            updateTaskListState (State.Loading newList) session

        State.Loaded _ ->
            updateTaskListState (State.Loaded newList) session



-- PRIVATE


updateTaskListState : State TaskList -> Session -> Session
updateTaskListState taskListState (Session model) =
    Session { model | taskList = taskListState }
