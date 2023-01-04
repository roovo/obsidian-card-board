module Column.UntaggedTests exposing (suite)

import Column.Untagged as UntaggedColumn
import ColumnNames exposing (ColumnNames)
import Expect
import TagBoard
import TaskList
import Test exposing (..)


suite : Test
suite =
    concat
        [ init
        , isEnabled
        , name
        ]


init : Test
init =
    describe "init"
        [ test "initializes with an empty TaskList" <|
            \() ->
                UntaggedColumn.init defaultTagBoardConfig defaultColumnNames
                    |> UntaggedColumn.taskList
                    |> Expect.equal TaskList.empty
        ]


isEnabled : Test
isEnabled =
    describe "isEnabled"
        [ test "returns True if the config.includeUntagged is True" <|
            \() ->
                UntaggedColumn.init
                    { defaultTagBoardConfig | includeUntagged = True }
                    defaultColumnNames
                    |> UntaggedColumn.isEnabled
                    |> Expect.equal True
        , test "returns False if the config.includeUntagged is False" <|
            \() ->
                UntaggedColumn.init
                    { defaultTagBoardConfig | includeUntagged = False }
                    defaultColumnNames
                    |> UntaggedColumn.isEnabled
                    |> Expect.equal False
        ]


name : Test
name =
    describe "name"
        [ test "defaults to 'Untagged'" <|
            \() ->
                UntaggedColumn.init
                    defaultTagBoardConfig
                    defaultColumnNames
                    |> UntaggedColumn.name
                    |> Expect.equal "Untagged"
        , test "can be customized" <|
            \() ->
                UntaggedColumn.init
                    defaultTagBoardConfig
                    { defaultColumnNames | untagged = Just "Foo" }
                    |> UntaggedColumn.name
                    |> Expect.equal "Foo"
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultTagBoardConfig : TagBoard.Config
defaultTagBoardConfig =
    TagBoard.defaultConfig
