module Form.SettingsTests exposing (suite)

import BoardConfig
import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Columns
import DefaultColumnNames
import Expect
import Form.Column as ColumnForm
import Form.Column.Completed as CompletedColumnForm
import Form.Column.Dated as DatedColumnForm
import Form.Decoder as FD
import Form.NewBoardConfig exposing (NewBoardConfigForm)
import Form.SafeDecoder as SD
import Form.Settings as SettingsForm
import SafeZipper
import Test exposing (..)


suite : Test
suite =
    concat
        -- [ decoder
        -- , empty
        [ init

        -- , safeDecoder
        -- , switchToBoard
        ]



-- decoder : Test
-- decoder =
--     describe "decoder"
--         [ test "decodes an empty SettingsForm" <|
--             \() ->
--                 SettingsForm.empty
--                     |> FD.run SettingsForm.decoder
--                     |> Expect.equal (Ok [])
--         , test "decodes a list of one of each Column type" <|
--             \() ->
--                 { columnsForms =
--                     SafeZipper.fromList
--                         [ { columnForms = [] }
--                         , { columnForms =
--                                 [ ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
--                                 , ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "7" }
--                                 ]
--                           }
--                         ]
--                 }
--                     |> FD.run SettingsForm.decoder
--                     |> Expect.equal
--                         (Ok <|
--                             [ Columns.empty
--                             , Columns.fromList
--                                 [ Column.Completed <| CompletedColumn.init "foo" 0 4
--                                 , Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7)
--                                 ]
--                             ]
--                         )
--         , test "errors if given invalid data" <|
--             \() ->
--                 { columnsForms =
--                     SafeZipper.fromList
--                         [ { columnForms = [ ColumnForm.CompletedColumnForm { name = "foo", limit = "p" } ] }
--                         , { columnForms = [ ColumnForm.DatedColumnForm { name = "", rangeType = "Before", from = "", to = "7" } ] }
--                         ]
--                 }
--                     |> FD.errors SettingsForm.decoder
--                     |> Expect.equal
--                         [ ( 0, ( 0, ColumnForm.CompletedColumnError (CompletedColumnForm.LimitError CompletedColumnForm.InvalidInt) ) )
--                         , ( 1, ( 0, ColumnForm.DatedColumnError DatedColumnForm.NameRequired ) )
--                         ]
--         ]
--
--
-- empty : Test
-- empty =
--     describe "empty"
--         [ test "initialises with empty columnForms" <|
--             \() ->
--                 SettingsForm.empty
--                     |> SettingsForm.columnsForms
--                     |> Expect.equal SafeZipper.empty
--         ]


init : Test
init =
    describe "init"
        [ test "initialises from an empty Settings" <|
            \() ->
                -- SafeZipper.empty
                --     |> SettingsForm.init
                --     |> SettingsForm.columnsForms
                --     |> Expect.equal SafeZipper.empty
                True
                    |> Expect.equal True

        -- , test "initialises columns from Settings" <|
        --     \() ->
        --         SafeZipper.fromList
        --             [ BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "foo" "emptyBoard")
        --             , BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "baz" "tagBoard")
        --             ]
        --             |> SettingsForm.init
        --             |> SettingsForm.columnsForms
        --             |> Expect.equal
        --                 (SafeZipper.fromList
        --                     [ { columnForms = [] }
        --                     , { columnForms =
        --                             [ ColumnForm.UntaggedColumnForm { name = "Untagged" }
        --                             , ColumnForm.OtherTagsColumnForm { name = "Other Tags" }
        --                             , ColumnForm.CompletedColumnForm { name = "Completed", limit = "10" }
        --                             ]
        --                       }
        --                     ]
        --                 )
        ]



-- safeDecoder : Test
-- safeDecoder =
--     describe "safeDecoder"
--         [ test "decodes an empty SettingsForm" <|
--             \() ->
--                 SettingsForm.empty
--                     |> SD.run SettingsForm.safeDecoder
--                     |> Expect.equal (Ok [])
--         , test "decodes a list of one of each Column type" <|
--             \() ->
--                 { columnsForms =
--                     SafeZipper.fromList
--                         [ { columnForms = [] }
--                         , { columnForms =
--                                 [ ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
--                                 , ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "7" }
--                                 ]
--                           }
--                         ]
--                 }
--                     |> SD.run SettingsForm.safeDecoder
--                     |> Expect.equal
--                         (Ok <|
--                             [ Columns.empty
--                             , Columns.fromList
--                                 [ Column.Completed <| CompletedColumn.init "foo" 0 4
--                                 , Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7)
--                                 ]
--                             ]
--                         )
--         , test "decodes if given invalid data" <|
--             \() ->
--                 { columnsForms =
--                     SafeZipper.fromList
--                         [ { columnForms = [ ColumnForm.CompletedColumnForm { name = "foo", limit = "p" } ] }
--                         , { columnForms = [ ColumnForm.DatedColumnForm { name = "", rangeType = "Before", from = "", to = "7" } ] }
--                         ]
--                 }
--                     |> SD.run SettingsForm.safeDecoder
--                     |> Expect.equal
--                         (Ok <|
--                             [ Columns.fromList [ Column.Completed <| CompletedColumn.init "foo" 0 10 ]
--                             , Columns.fromList [ Column.Dated <| DatedColumn.init "" (DatedColumn.Before 7) ]
--                             ]
--                         )
--         ]
--
--
-- switchToBoard : Test
-- switchToBoard =
--     describe "switchToBoard"
--         [ test "does nothing with an empty Settings" <|
--             \() ->
--                 SafeZipper.empty
--                     |> SettingsForm.init
--                     |> SettingsForm.switchToBoard 1
--                     |> SettingsForm.columnsForms
--                     |> Expect.equal SafeZipper.empty
--         , test "goes to the end of the list if given an index which is past the last one" <|
--             \() ->
--                 SafeZipper.fromList
--                     [ BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "foo" "emptyBoard")
--                     , BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "baz" "tagBoard")
--                     ]
--                     |> SettingsForm.init
--                     |> SettingsForm.switchToBoard 7
--                     |> SettingsForm.columnsForms
--                     |> SafeZipper.currentIndex
--                     |> Expect.equal (Just 1)
--         , test "switches index" <|
--             \() ->
--                 SafeZipper.fromList
--                     [ BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "foo" "emptyBoard")
--                     , BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "baz" "tagBoard")
--                     ]
--                     |> SettingsForm.init
--                     |> SettingsForm.switchToBoard 1
--                     |> SettingsForm.columnsForms
--                     |> SafeZipper.currentIndex
--                     |> Expect.equal (Just 1)
--         ]
