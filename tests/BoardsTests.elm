module BoardsTests exposing (suite)

import Board
import BoardConfig
import Boards
import ColumnNames
import Expect
import Helpers.TaskListHelpers as TaskListHelpers
import SafeZipper
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ boardZipper
        , currentIndex
        , length
        , titles
        ]


boardZipper : Test
boardZipper =
    describe "boardZipper"
        [ test "returns an empty SafeZipper if there are no boards configured" <|
            \() ->
                Boards.init ColumnNames.default SafeZipper.empty TaskListHelpers.taskListFromFileG
                    |> Boards.boardZipper
                    |> Expect.equal SafeZipper.empty
        , test "returns empty boards as a SafeZipper if there are some boards configured but the TaskList is empty" <|
            \() ->
                Boards.init ColumnNames.default (SafeZipper.fromList [ BoardConfig.fromBoardType "dateBoard" "dateboard title" ]) TaskList.empty
                    |> Boards.boardZipper
                    |> Expect.equal (SafeZipper.fromList [ Board.init ColumnNames.default (BoardConfig.fromBoardType "dateBoard" "dateboard title") TaskList.empty ])
        , test "returns boards (containing all TaskItems) as a SafeZipper if there are boards configured and ther are tasks" <|
            \() ->
                Boards.init ColumnNames.default (SafeZipper.fromList [ BoardConfig.fromBoardType "dateBoard" "dateboard title" ]) TaskListHelpers.taskListFromFileG
                    |> Boards.boardZipper
                    |> Expect.equal (SafeZipper.fromList [ Board.init ColumnNames.default (BoardConfig.fromBoardType "dateBoard" "dateboard title") TaskListHelpers.taskListFromFileG ])
        ]


currentIndex : Test
currentIndex =
    describe "currentIndex"
        [ test "returns Nothing if there are no boards configured" <|
            \() ->
                Boards.init ColumnNames.default SafeZipper.empty TaskList.empty
                    |> Boards.currentIndex
                    |> Expect.equal Nothing
        , test "returns 0 if there are boards and the current index of the configs is 0" <|
            \() ->
                Boards.init ColumnNames.default (SafeZipper.fromList [ BoardConfig.fromBoardType "dateBoard" "dateboard title", BoardConfig.fromBoardType "dateBoard" "another dateboard title" ]) TaskList.empty
                    |> Boards.currentIndex
                    |> Expect.equal (Just 0)
        , test "returns 1 if there are boards and the current index of the configs is 1" <|
            \() ->
                Boards.init ColumnNames.default (SafeZipper.next <| SafeZipper.fromList [ BoardConfig.fromBoardType "dateBoard" "dateboard title", BoardConfig.fromBoardType "dateBoard" "another dateboard title" ]) TaskList.empty
                    |> Boards.currentIndex
                    |> Expect.equal (Just 1)
        ]


length : Test
length =
    describe "length"
        [ test "returns 0 if there are no boards configured" <|
            \() ->
                Boards.init ColumnNames.default SafeZipper.empty TaskList.empty
                    |> Boards.length
                    |> Expect.equal 0
        , test "returns 2 if there are 2 boards in the configs" <|
            \() ->
                Boards.init ColumnNames.default (SafeZipper.next <| SafeZipper.fromList [ BoardConfig.fromBoardType "dateBoard" "dateboard title", BoardConfig.fromBoardType "dateBoard" "another dateboard title" ]) TaskList.empty
                    |> Boards.length
                    |> Expect.equal 2
        ]


titles : Test
titles =
    describe "titles"
        [ test "returns an empty SafeZipper if there are no boards configured" <|
            \() ->
                Boards.init ColumnNames.default SafeZipper.empty TaskList.empty
                    |> Boards.titles
                    |> Expect.equal SafeZipper.empty
        , test "returns the bopard titles in a SafeZipper if there are some boards configured" <|
            \() ->
                Boards.init ColumnNames.default (SafeZipper.fromList [ BoardConfig.fromBoardType "dateBoard" "dateboard title", BoardConfig.fromBoardType "dateBoard" "another dateboard title" ]) TaskList.empty
                    |> Boards.titles
                    |> Expect.equal (SafeZipper.fromList [ "dateboard title", "another dateboard title" ])
        ]
