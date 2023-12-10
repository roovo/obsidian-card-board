module BoardsTests exposing (suite)

import Board
import BoardConfig exposing (BoardConfig)
import Boards
import DefaultColumnNames
import Expect
import Helpers.TaskListHelpers as TaskListHelpers
import NewBoardConfig exposing (NewBoardConfig)
import SafeZipper
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ boardZipper

        -- , cards
        , currentIndex
        , length
        , names
        ]


boardZipper : Test
boardZipper =
    describe "boardZipper"
        [ test "returns an empty SafeZipper if there are no boards configured" <|
            \() ->
                Boards.init "b1" SafeZipper.empty TaskListHelpers.taskListFromFileG
                    |> Boards.boardZipper
                    |> Expect.equal SafeZipper.empty
        , test "returns empty boards as a SafeZipper if there are some boards configured but the TaskList is empty" <|
            \() ->
                let
                    dateBoard : BoardConfig
                    dateBoard =
                        NewBoardConfig "foo" "dateBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                in
                Boards.init "b1" (SafeZipper.fromList [ dateBoard ]) TaskList.empty
                    |> Boards.boardZipper
                    |> Expect.equal
                        (SafeZipper.fromList [ Board.init "b1" dateBoard TaskList.empty ])
        , test "returns boards (containing all TaskItems) as a SafeZipper if there are boards configured and ther are tasks" <|
            \() ->
                let
                    dateBoard : BoardConfig
                    dateBoard =
                        NewBoardConfig "foo" "dateBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                in
                Boards.init "b1"
                    (SafeZipper.fromList [ dateBoard ])
                    TaskListHelpers.taskListFromFileG
                    |> Boards.boardZipper
                    |> Expect.equal
                        (SafeZipper.fromList
                            [ Board.init "b1" dateBoard TaskListHelpers.taskListFromFileG ]
                        )
        ]


currentIndex : Test
currentIndex =
    describe "currentIndex"
        [ test "returns Nothing if there are no boards configured" <|
            \() ->
                Boards.init "b1" SafeZipper.empty TaskList.empty
                    |> Boards.currentIndex
                    |> Expect.equal Nothing
        , test "returns 0 if there are boards and the current index of the configs is 0" <|
            \() ->
                let
                    dateBoard : BoardConfig
                    dateBoard =
                        NewBoardConfig "foo" "dateBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default

                    emptyBoard : BoardConfig
                    emptyBoard =
                        NewBoardConfig "bar" "emptyBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                in
                Boards.init "b1"
                    (SafeZipper.fromList [ dateBoard, emptyBoard ])
                    TaskList.empty
                    |> Boards.currentIndex
                    |> Expect.equal (Just 0)
        , test "returns 1 if there are boards and the current index of the configs is 1" <|
            \() ->
                let
                    dateBoard : BoardConfig
                    dateBoard =
                        NewBoardConfig "foo" "dateBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default

                    emptyBoard : BoardConfig
                    emptyBoard =
                        NewBoardConfig "bar" "emptyBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                in
                Boards.init "b1"
                    (SafeZipper.next <| SafeZipper.fromList [ dateBoard, emptyBoard ])
                    TaskList.empty
                    |> Boards.currentIndex
                    |> Expect.equal (Just 1)
        ]


length : Test
length =
    describe "length"
        [ test "returns 0 if there are no boards configured" <|
            \() ->
                Boards.init "b1" SafeZipper.empty TaskList.empty
                    |> Boards.length
                    |> Expect.equal 0
        , test "returns 2 if there are 2 boards in the configs" <|
            \() ->
                let
                    dateBoard : BoardConfig
                    dateBoard =
                        NewBoardConfig "foo" "dateBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default

                    emptyBoard : BoardConfig
                    emptyBoard =
                        NewBoardConfig "bar" "emptyBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                in
                Boards.init "b1"
                    (SafeZipper.next <| SafeZipper.fromList [ dateBoard, emptyBoard ])
                    TaskList.empty
                    |> Boards.length
                    |> Expect.equal 2
        ]


names : Test
names =
    describe "names"
        [ test "returns the board names in a SafeZipper if there are some boards configured" <|
            \() ->
                let
                    dateBoard : BoardConfig
                    dateBoard =
                        NewBoardConfig "foo" "dateBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default

                    emptyBoard : BoardConfig
                    emptyBoard =
                        NewBoardConfig "bar" "emptyBoard"
                            |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                in
                Boards.init "b1"
                    (SafeZipper.fromList [ dateBoard, emptyBoard ])
                    TaskList.empty
                    |> Boards.names
                    |> Expect.equal (SafeZipper.fromList [ "foo", "bar" ])
        ]
