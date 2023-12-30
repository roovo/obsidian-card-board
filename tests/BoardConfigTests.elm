module BoardConfigTests exposing (suite)

import BoardConfig
import Column
import Columns
import DefaultColumnNames exposing (DefaultColumnNames)
import Expect
import Filter
import Form.BoardConfig as BoardConfigForm
import Form.NewBoard exposing (NewBoardForm)
import Form.SafeDecoder as SD
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ collapseColumn
        , encodeDecode
        , mapFilters
        , setNamesToDefault
        , updateName
        ]


collapseColumn : Test
collapseColumn =
    describe "collapseColumn"
        [ test "collapses a column" <|
            \() ->
                { columns =
                    Columns.fromList
                        [ Column.undated "one"
                        , Column.untagged "two"
                        , Column.otherTags "three" []
                        ]
                , filters = []
                , filterPolarity = Filter.Allow
                , filterScope = Filter.Both
                , name = ""
                , showColumnTags = False
                , showFilteredTags = False
                }
                    |> BoardConfig.fromConfig
                    |> BoardConfig.collapseColumn 1 True
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.isCollapsed
                    |> Expect.equal [ False, True, False ]
        , test "uncollapses a column" <|
            \() ->
                { columns =
                    Columns.fromList
                        [ Column.undated "one"
                        , Column.untagged "two"
                        , Column.otherTags "three" []
                        ]
                , filters = []
                , filterPolarity = Filter.Allow
                , filterScope = Filter.Both
                , name = ""
                , showColumnTags = False
                , showFilteredTags = False
                }
                    |> BoardConfig.fromConfig
                    |> BoardConfig.collapseColumn 1 True
                    |> BoardConfig.collapseColumn 1 False
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.isCollapsed
                    |> Expect.equal [ False, False, False ]
        ]


encodeDecode : Test
encodeDecode =
    describe "encoding and decoding config"
        [ test "can decode an encoded string back to the original" <|
            \() ->
                BoardConfigHelpers.exampleBoardConfig
                    |> TsEncode.runExample BoardConfig.encoder
                    |> .output
                    |> DecodeHelpers.runDecoder BoardConfig.decoder_v_0_11_0
                    |> .decoded
                    |> Expect.equal (Ok BoardConfigHelpers.exampleBoardConfig)
        ]


mapFilters : Test
mapFilters =
    describe "mapFilters"
        [ test "updates the filters" <|
            \() ->
                { columns =
                    Columns.fromList
                        [ Column.undated "one"
                        , Column.untagged "two"
                        , Column.otherTags "three" []
                        ]
                , filters = FilterHelpers.exampleFilters
                , filterPolarity = Filter.Allow
                , filterScope = Filter.Both
                , name = ""
                , showColumnTags = False
                , showFilteredTags = False
                }
                    |> BoardConfig.fromConfig
                    |> BoardConfig.mapFilters (always Filter.dummy)
                    |> BoardConfig.filters
                    |> List.map Filter.value
                    |> Expect.equal [ "", "", "" ]
        ]


setNamesToDefault : Test
setNamesToDefault =
    describe "setNamesToDefault"
        [ test "can set standard date board column names to the given defaults" <|
            \() ->
                NewBoardForm "foo" "dateBoard"
                    |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map (BoardConfig.setNamesToDefault customColumnNames)
                    |> Result.map BoardConfig.columns
                    |> Result.map Columns.toList
                    |> Result.withDefault []
                    |> List.map Column.name
                    |> Expect.equal [ "No Date", "This Day", "The Morrow", "Way Out", "Done" ]
        , test "can set standard tag board column names to the given defaults" <|
            \() ->
                NewBoardForm "foo" "tagBoard"
                    |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map (BoardConfig.setNamesToDefault customColumnNames)
                    |> Result.map BoardConfig.columns
                    |> Result.map Columns.toList
                    |> Result.withDefault []
                    |> List.map Column.name
                    |> Expect.equal [ "No Tags", "Other Taggy-Waggys", "Done" ]
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the title for a DateBoard config" <|
            \() ->
                { columns = Columns.empty
                , filters = []
                , filterPolarity = Filter.Allow
                , filterScope = Filter.Both
                , name = ""
                , showColumnTags = False
                , showFilteredTags = False
                }
                    |> BoardConfig.fromConfig
                    |> BoardConfig.updateName "new title"
                    |> BoardConfig.name
                    |> Expect.equal "new title"
        ]



-- HELPERS


customColumnNames : DefaultColumnNames
customColumnNames =
    { today = Just "This Day"
    , tomorrow = Just "The Morrow"
    , future = Just "Way Out"
    , undated = Just "No Date"
    , otherTags = Just "Other Taggy-Waggys"
    , untagged = Just "No Tags"
    , completed = Just "Done"
    }
