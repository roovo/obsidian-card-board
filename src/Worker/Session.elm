module Worker.Session exposing
    ( Session
    , addTaskList
    , dataviewTaskCompletion
    , default
    , deleteItemsFromFile
    , finishAdding
    , fromFlags
    , replaceTaskItems
    , taskList
    )

import DataviewTaskCompletion exposing (DataviewTaskCompletion)
import InteropDefinitions
import State exposing (State)
import TaskList exposing (TaskList)



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



-- PRIVATE


updateTaskListState : State TaskList -> Session -> Session
updateTaskListState taskListState (Session model) =
    Session { model | taskList = taskListState }
