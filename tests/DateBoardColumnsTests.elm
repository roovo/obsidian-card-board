module DateBoardColumnsTests exposing (suite)

import Column
import ColumnNames exposing (ColumnNames)
import Date exposing (Date)
import DateBoardColumns exposing (DateBoardColumns)
import DateBoardConfig exposing (DateBoardConfig)
import Expect
import Helpers.BoardHelpers as BoardHelpers
import Helpers.DateTimeHelpers as DateTimeHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import TaskItem exposing (TaskItem)
import Test exposing (..)


suite : Test
suite =
    concat
        [ addTaskItem
        , columns
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "adds an incomplete task item with no due date to the undated column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem "- [ ] foo")
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add a completed task item with no due date to the undated column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem "- [x] foo")
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Undated"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        , test "adds an incomplete task item which is due today to the today column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem ("- [ ] foo " ++ dueString 0))
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Today"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds an incomplete task item from 4 days ago to the today column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem ("- [ ] foo " ++ dueString -4))
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Today"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds an incomplete task item which is due tomorrow to the tomorrow column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem ("- [ ] foo " ++ dueString 1))
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Tomorrow"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds an incomplete task item which is due the day after tomorrow to the future column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem ("- [ ] foo " ++ dueString 2))
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Future"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds a completed task item with no due date to the completed column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem "- [x] foo")
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds a completed task item which is due tomorrow to the completed column" <|
            \() ->
                emptyDateBoardColumns
                    |> DateBoardColumns.addTaskItem (taskItem ("- [x] foo " ++ dueString 1))
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add a completed task item with no due date to the completed column if includeUndated is False" <|
            \() ->
                DateBoardColumns.init
                    today
                    ColumnNames.default
                    { defaultConfig | includeUndated = False }
                    |> DateBoardColumns.addTaskItem (taskItem "- [x] foo")
                    |> DateBoardColumns.columns
                    |> BoardHelpers.thingsInColumn "Completed"
                    |> List.map TaskItem.title
                    |> Expect.equal []
        ]


columns : Test
columns =
    describe "columns"
        [ test "default columns are just for undated today tomorrow future and completed" <|
            \() ->
                DateBoardColumns.init today defaultColumnNames defaultConfig
                    |> DateBoardColumns.columns
                    |> List.map Column.name
                    |> Expect.equal [ "Undated", "Today", "Tomorrow", "Future", "Completed" ]
        , test "returns columns as defined in the config" <|
            \() ->
                DateBoardColumns.init today defaultColumnNames { defaultConfig | completedCount = 0, includeUndated = False }
                    |> DateBoardColumns.columns
                    |> List.map Column.name
                    |> Expect.equal [ "Today", "Tomorrow", "Future" ]
        , test "allows the default date column names to be changed" <|
            \() ->
                DateBoardColumns.init
                    today
                    { defaultColumnNames | today = Just "Foo", tomorrow = Just "Bar", future = Just "Baz" }
                    { defaultConfig | completedCount = 0, includeUndated = False }
                    |> DateBoardColumns.columns
                    |> List.map Column.name
                    |> Expect.equal [ "Foo", "Bar", "Baz" ]
        ]



-- HELPERS


defaultConfig : DateBoardConfig
defaultConfig =
    DateBoardConfig.default


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


dueString : Int -> String
dueString offset =
    "@due(" ++ DateTimeHelpers.offsetDateString offset ++ ")"


emptyDateBoardColumns : DateBoardColumns
emptyDateBoardColumns =
    DateBoardColumns.init
        today
        ColumnNames.default
        { defaultConfig | includeUndated = True }


today : Date
today =
    DateTimeHelpers.todayDate


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
