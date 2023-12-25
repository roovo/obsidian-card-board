module Form.BoardConfigsTests exposing (suite)

import BoardConfig
import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Columns
import DefaultColumnNames
import Expect
import Form.BoardConfigs as BoardConfigsForm
import Form.Column as ColumnForm
import Form.CompletedColumn as CompletedColumnForm
import Form.DatedColumn as DatedColumnForm
import Form.Decoder as FD
import Form.SafeDecoder as SD
import NewBoardConfig exposing (NewBoardConfig)
import SafeZipper
import Test exposing (..)


suite : Test
suite =
    concat
        [ decoder
        , empty
        , init
        , safeDecoder
        , switchToBoard
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes an empty BoardConfigsForm" <|
            \() ->
                BoardConfigsForm.empty
                    |> FD.run BoardConfigsForm.decoder
                    |> Expect.equal (Ok [])
        , test "decodes a list of one of each Column type" <|
            \() ->
                { columnsForms =
                    SafeZipper.fromList
                        [ { columnForms = [] }
                        , { columnForms =
                                [ ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
                                , ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "7" }
                                ]
                          }
                        ]
                }
                    |> FD.run BoardConfigsForm.decoder
                    |> Expect.equal
                        (Ok <|
                            [ Columns.empty
                            , Columns.fromList
                                [ Column.Completed <| CompletedColumn.init "foo" 0 4
                                , Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7)
                                ]
                            ]
                        )
        , test "errors if given invalid data" <|
            \() ->
                { columnsForms =
                    SafeZipper.fromList
                        [ { columnForms = [ ColumnForm.CompletedColumnForm { name = "foo", limit = "p" } ] }
                        , { columnForms = [ ColumnForm.DatedColumnForm { name = "", rangeType = "Before", from = "", to = "7" } ] }
                        ]
                }
                    |> FD.errors BoardConfigsForm.decoder
                    |> Expect.equal
                        [ ( 0, ( 0, ColumnForm.CompletedColumnError (CompletedColumnForm.LimitError CompletedColumnForm.InvalidInt) ) )
                        , ( 1, ( 0, ColumnForm.DatedColumnError DatedColumnForm.NameRequired ) )
                        ]
        ]


empty : Test
empty =
    describe "empty"
        [ test "initialises with empty columnForms" <|
            \() ->
                BoardConfigsForm.empty
                    |> BoardConfigsForm.columnsForms
                    |> Expect.equal SafeZipper.empty
        ]


init : Test
init =
    describe "init"
        [ test "initialises from an empty BoardConfigs" <|
            \() ->
                SafeZipper.empty
                    |> BoardConfigsForm.init
                    |> BoardConfigsForm.columnsForms
                    |> Expect.equal SafeZipper.empty
        , test "initialises columns from BoardConfigs" <|
            \() ->
                SafeZipper.fromList
                    [ BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "foo" "emptyBoard")
                    , BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "baz" "tagBoard")
                    ]
                    |> BoardConfigsForm.init
                    |> BoardConfigsForm.columnsForms
                    |> Expect.equal
                        (SafeZipper.fromList
                            [ { columnForms = [] }
                            , { columnForms =
                                    [ ColumnForm.UntaggedColumnForm { name = "Untagged" }
                                    , ColumnForm.OtherTagsColumnForm { name = "Other Tags" }
                                    , ColumnForm.CompletedColumnForm { name = "Completed", limit = "10" }
                                    ]
                              }
                            ]
                        )
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes an empty BoardConfigsForm" <|
            \() ->
                BoardConfigsForm.empty
                    |> SD.run BoardConfigsForm.safeDecoder
                    |> Expect.equal (Ok [])
        , test "decodes a list of one of each Column type" <|
            \() ->
                { columnsForms =
                    SafeZipper.fromList
                        [ { columnForms = [] }
                        , { columnForms =
                                [ ColumnForm.CompletedColumnForm { name = "foo", limit = "4" }
                                , ColumnForm.DatedColumnForm { name = "foo", rangeType = "Before", from = "", to = "7" }
                                ]
                          }
                        ]
                }
                    |> SD.run BoardConfigsForm.safeDecoder
                    |> Expect.equal
                        (Ok <|
                            [ Columns.empty
                            , Columns.fromList
                                [ Column.Completed <| CompletedColumn.init "foo" 0 4
                                , Column.Dated <| DatedColumn.init "foo" (DatedColumn.Before 7)
                                ]
                            ]
                        )
        , test "decodes if given invalid data" <|
            \() ->
                { columnsForms =
                    SafeZipper.fromList
                        [ { columnForms = [ ColumnForm.CompletedColumnForm { name = "foo", limit = "p" } ] }
                        , { columnForms = [ ColumnForm.DatedColumnForm { name = "", rangeType = "Before", from = "", to = "7" } ] }
                        ]
                }
                    |> SD.run BoardConfigsForm.safeDecoder
                    |> Expect.equal
                        (Ok <|
                            [ Columns.fromList [ Column.Completed <| CompletedColumn.init "foo" 0 10 ]
                            , Columns.fromList [ Column.Dated <| DatedColumn.init "" (DatedColumn.Before 7) ]
                            ]
                        )
        ]


switchToBoard : Test
switchToBoard =
    describe "switchToBoard"
        [ test "does nothing with an empty BoardConfigs" <|
            \() ->
                SafeZipper.empty
                    |> BoardConfigsForm.init
                    |> BoardConfigsForm.switchToBoard 1
                    |> BoardConfigsForm.columnsForms
                    |> Expect.equal SafeZipper.empty
        , test "goes to the end of the list if given an index which is past the last one" <|
            \() ->
                SafeZipper.fromList
                    [ BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "foo" "emptyBoard")
                    , BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "baz" "tagBoard")
                    ]
                    |> BoardConfigsForm.init
                    |> BoardConfigsForm.switchToBoard 7
                    |> BoardConfigsForm.columnsForms
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 1)
        , test "switches index" <|
            \() ->
                SafeZipper.fromList
                    [ BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "foo" "emptyBoard")
                    , BoardConfig.fromNewBoardConfig DefaultColumnNames.default (NewBoardConfig "baz" "tagBoard")
                    ]
                    |> BoardConfigsForm.init
                    |> BoardConfigsForm.switchToBoard 1
                    |> BoardConfigsForm.columnsForms
                    |> SafeZipper.currentIndex
                    |> Expect.equal (Just 1)
        ]
