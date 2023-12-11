module BoardConfigTests exposing (suite)

import BoardConfig
import Column
import Column.Dated as DatedColumn
import Columns
import DefaultColumnNames exposing (DefaultColumnNames)
import Expect
import Filter
import Helpers.BoardConfigHelpers as BoardConfigHelpers
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import List.Extra as LE
import Maybe.Extra as ME
import NewBoardConfig exposing (NewBoardConfig)
import NewColumnConfig exposing (NewColumnConfig)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ addColumn
        , collapseColumn
        , encodeDecode
        , fromNewBoardConfig
        , mapFilters
        , setNamesToDefault
        , toggleShowColumnTags
        , toggleShowFilteredTags
        , toggleTagFilterScope
        , updateColumnName
        , updateCompletedColumnLimit
        , updateDatedColumnRangeType
        , updateDatedColumnRangeValueFrom
        , updateDatedColumnRangeValueTo
        , updateFilterPolarity
        , updateFilterScope
        , updateFilters
        , updateName
        ]


addColumn : Test
addColumn =
    describe "addColumn"
        [ test "adds an Undated columnn to an empty board" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.addColumn
                        DefaultColumnNames.default
                        (NewColumnConfig "Undated" "undated")
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "Undated" ]
        , test "adds an Undated  columnn after an existing Untagged column" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.addColumn
                        DefaultColumnNames.default
                        (NewColumnConfig "Untagged" "untagged")
                    |> BoardConfig.addColumn
                        DefaultColumnNames.default
                        (NewColumnConfig "Undated" "undated")
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "Undated" ]
        , test "adds a non-Completed columnn before the completed column" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.addColumn
                        DefaultColumnNames.default
                        (NewColumnConfig "Untagged" "untagged")
                    |> BoardConfig.addColumn
                        DefaultColumnNames.default
                        (NewColumnConfig "Completed" "completed")
                    |> BoardConfig.addColumn
                        DefaultColumnNames.default
                        (NewColumnConfig "Undated" "undated")
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "Untagged", "Undated", "Completed" ]
        ]


collapseColumn : Test
collapseColumn =
    describe "collapseColumn"
        [ test "collapses a column" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.collapseColumn 1 True
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.isCollapsed
                    |> Expect.equal [ False, True, False, False, False ]
        , test "UNcollapses a column" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.collapseColumn 1 True
                    |> BoardConfig.collapseColumn 1 False
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.isCollapsed
                    |> Expect.equal [ False, False, False, False, False ]
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


fromNewBoardConfig : Test
fromNewBoardConfig =
    describe "fromNewBoardConfig"
        [ describe "dateBoard"
            [ test "has the basic date columns" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.columns
                        |> Columns.toList
                        |> List.map Column.name
                        |> Expect.equal [ "Undated", "Today", "Tomorrow", "Future", "Completed" ]
            , test "can override the names of the basic date columns" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig customColumnNames
                        |> BoardConfig.columns
                        |> Columns.toList
                        |> List.map Column.name
                        |> Expect.equal [ "No Date", "This Day", "The Morrow", "Way Out", "Done" ]
            , test "has no filters" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filters
                        |> Expect.equal []
            , test "has the default filter polarity" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filterPolarity
                        |> Expect.equal Filter.defaultPolarity
            , test "has the default filter scope" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filterScope
                        |> Expect.equal Filter.defaultScope
            , test "sets the board name" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.name
                        |> Expect.equal "foo"
            , test "shows column tags" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.showColumnTags
                        |> Expect.equal True
            , test "shows filtered tags" <|
                \() ->
                    NewBoardConfig "foo" "dateBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.showFilteredTags
                        |> Expect.equal True
            ]
        , describe "emptyBoard"
            [ test "has no columns" <|
                \() ->
                    NewBoardConfig "foo" "emptyBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.columns
                        |> Expect.equal Columns.empty
            , test "has no filters" <|
                \() ->
                    NewBoardConfig "foo" "emptyBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filters
                        |> Expect.equal []
            , test "has the default filter polarity" <|
                \() ->
                    NewBoardConfig "foo" "emptyBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filterPolarity
                        |> Expect.equal Filter.defaultPolarity
            , test "has the default filter scope" <|
                \() ->
                    NewBoardConfig "foo" "emptyBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filterScope
                        |> Expect.equal Filter.defaultScope
            , test "sets the board name" <|
                \() ->
                    NewBoardConfig "foo" "emptyBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.name
                        |> Expect.equal "foo"
            , test "shows column tags" <|
                \() ->
                    NewBoardConfig "foo" "emptyBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.showColumnTags
                        |> Expect.equal True
            , test "shows filtered tags" <|
                \() ->
                    NewBoardConfig "foo" "emptyBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.showFilteredTags
                        |> Expect.equal True
            ]
        , describe "tagBoard"
            [ test "has the basic tag board columns" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.columns
                        |> Columns.toList
                        |> List.map Column.name
                        |> Expect.equal [ "Untagged", "Other Tags", "Completed" ]
            , test "can override the names of tag board columns" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig customColumnNames
                        |> BoardConfig.columns
                        |> Columns.toList
                        |> List.map Column.name
                        |> Expect.equal [ "No Tags", "Other Taggy-Waggys", "Done" ]
            , test "has no filters" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filters
                        |> Expect.equal []
            , test "has the default filter polarity" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filterPolarity
                        |> Expect.equal Filter.defaultPolarity
            , test "has the default filter scope" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.filterScope
                        |> Expect.equal Filter.defaultScope
            , test "sets the board name" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.name
                        |> Expect.equal "foo"
            , test "shows column tags" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.showColumnTags
                        |> Expect.equal True
            , test "shows filtered tags" <|
                \() ->
                    NewBoardConfig "foo" "tagBoard"
                        |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                        |> BoardConfig.showFilteredTags
                        |> Expect.equal True
            ]
        ]


mapFilters : Test
mapFilters =
    describe "mapFilters"
        [ test "updates the filters" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateFilters FilterHelpers.exampleFilters
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
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.setNamesToDefault customColumnNames
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "No Date", "This Day", "The Morrow", "Way Out", "Done" ]
        , test "can set standard tag board column names to the given defaults" <|
            \() ->
                NewBoardConfig "foo" "tagBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.setNamesToDefault customColumnNames
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "No Tags", "Other Taggy-Waggys", "Done" ]
        ]


toggleShowColumnTags : Test
toggleShowColumnTags =
    describe "toggleShowColumnTags"
        [ test "toggles showColumnTags" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.toggleShowColumnTags
                    |> BoardConfig.showColumnTags
                    |> Expect.equal False
        ]


toggleShowFilteredTags : Test
toggleShowFilteredTags =
    describe "toggleShowFilteredTags"
        [ test "toggles showFilteredTags" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.toggleShowFilteredTags
                    |> BoardConfig.showFilteredTags
                    |> Expect.equal False
        ]


toggleTagFilterScope : Test
toggleTagFilterScope =
    describe "toggleTagFilterScope"
        [ test "toggles Both -> TopLevelOnly" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.toggleTagFilterScope
                    |> BoardConfig.filterScope
                    |> Expect.equal Filter.TopLevelOnly
        , test "toggles TopLevelOnly -> SubTasksOnly" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.toggleTagFilterScope
                    |> BoardConfig.toggleTagFilterScope
                    |> BoardConfig.filterScope
                    |> Expect.equal Filter.SubTasksOnly
        , test "toggles SubTasksOnly -> Both" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.toggleTagFilterScope
                    |> BoardConfig.toggleTagFilterScope
                    |> BoardConfig.toggleTagFilterScope
                    |> BoardConfig.filterScope
                    |> Expect.equal Filter.Both
        ]


updateColumnName : Test
updateColumnName =
    describe "updateColumnName"
        [ test "updates a column name" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateColumnName 1 "new name"
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "Undated", "new name", "Tomorrow", "Future", "Completed" ]
        ]


updateCompletedColumnLimit : Test
updateCompletedColumnLimit =
    describe "updateCompletedColumnLimit"
        [ test "updates the limit" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateCompletedColumnLimit 4 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.completedLimit
                    |> Expect.equal 7
        , test "does nothing if given the wrong index" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateCompletedColumnLimit 0 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.completedLimit
                    |> Expect.equal 10
        , test "does nothing if given Nothing" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateCompletedColumnLimit 4 Nothing
                    |> BoardConfig.columns
                    |> Columns.completedLimit
                    |> Expect.equal 10
        ]


updateDatedColumnRangeType : Test
updateDatedColumnRangeType =
    describe "updateDatedColumnRangeType"
        [ test "updates the limit" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeType 1 "After"
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 1
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.After 1)
        , test "does nothing if given the wrong index" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeType 0 "After"
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 1
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.Before 1)
        , test "does nothing if given an invalid range type" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeType 1 "Xxxx"
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 1
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.Before 1)
        ]


updateDatedColumnRangeValueFrom : Test
updateDatedColumnRangeValueFrom =
    describe "updateDatedColumnRangeValueFrom"
        [ test "updates the limit for an After range" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueFrom 3 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 3
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.After 7)
        , test "does nothing if given a column with a Before range" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueFrom 1 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 1
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.Before 1)
        , test "updates the from part of a Between range" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueFrom 2 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 2
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.Between { from = 7, to = 1 })
        , test "does nothing if given Nothing" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueFrom 3 Nothing
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 3
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.After 1)
        ]


updateDatedColumnRangeValueTo : Test
updateDatedColumnRangeValueTo =
    describe "updateDatedColumnRangeValueTo"
        [ test "updates the limit for a Before range" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueTo 1 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 1
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.Before 7)
        , test "does nothing if given a column with an After range" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueTo 3 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 3
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.After 1)
        , test "updates the to part of a Between range" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueTo 2 (Just 7)
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 2
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.Between { from = 1, to = 7 })
        , test "does nothing if given Nothing" <|
            \() ->
                NewBoardConfig "foo" "dateBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateDatedColumnRangeValueTo 1 Nothing
                    |> BoardConfig.columns
                    |> Columns.toList
                    |> LE.getAt 1
                    |> Maybe.map Column.asDatedColumn
                    |> ME.join
                    |> Maybe.map DatedColumn.range
                    |> Expect.equal (Just <| DatedColumn.Before 1)
        ]


updateFilterPolarity : Test
updateFilterPolarity =
    describe "updateFilterPolarity"
        [ test "updates the filter polarity" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateFilterPolarity "Deny"
                    |> BoardConfig.filterPolarity
                    |> Expect.equal Filter.Deny
        ]


updateFilterScope : Test
updateFilterScope =
    describe "updateFilterScope"
        [ test "updates the filter scope" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateFilterScope Filter.SubTasksOnly
                    |> BoardConfig.filterScope
                    |> Expect.equal Filter.SubTasksOnly
        ]


updateFilters : Test
updateFilters =
    describe "updateFilters"
        [ test "updates the filters for a DateBoard config" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
                    |> BoardConfig.updateFilters FilterHelpers.exampleFilters
                    |> BoardConfig.filters
                    |> Expect.equal FilterHelpers.exampleFilters
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the title for a DateBoard config" <|
            \() ->
                NewBoardConfig "foo" "emptyBoard"
                    |> BoardConfig.fromNewBoardConfig DefaultColumnNames.default
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
