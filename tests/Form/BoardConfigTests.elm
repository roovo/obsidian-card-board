module Form.BoardConfigTests exposing (suite)

import BoardConfig
import Column
import Column.Completed as CompletedColumn
import Columns
import Expect
import Filter
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Columns as ColumnsForm
import Form.SafeDecoder as SD
import Helpers.FilterHelpers as FilterHelpers
import Test exposing (..)


suite : Test
suite =
    concat
        [ safeDecoder
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes the columns" <|
            \() ->
                { exampleBoardConfigForm
                    | columns =
                        ColumnsForm.init <|
                            Columns.fromList [ Column.completed <| CompletedColumn.init "" 0 10 ]
                }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.columns
                    |> Expect.equal (Ok <| Columns.fromList [ Column.completed <| CompletedColumn.init "" 0 10 ])
        , test "decodes the filters" <|
            \() ->
                { exampleBoardConfigForm | filters = FilterHelpers.exampleFilters }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filters
                    |> Expect.equal (Ok <| FilterHelpers.exampleFilters)
        , test "decodes the Allow filterPolarity" <|
            \() ->
                { exampleBoardConfigForm | filterPolarity = "Allow" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filterPolarity
                    |> Expect.equal (Ok <| Filter.Allow)
        , test "decodes the Deny filterPolarity" <|
            \() ->
                { exampleBoardConfigForm | filterPolarity = "Deny" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filterPolarity
                    |> Expect.equal (Ok <| Filter.Deny)
        , test "decodes an unrecognised filterPolarity as Allow" <|
            \() ->
                { exampleBoardConfigForm | filterPolarity = "xxxxxx" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filterPolarity
                    |> Expect.equal (Ok <| Filter.Allow)
        , test "decodes the TopLevelOnly filterScope" <|
            \() ->
                { exampleBoardConfigForm | filterScope = "TopLevelOnly" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filterScope
                    |> Expect.equal (Ok <| Filter.TopLevelOnly)
        , test "decodes the SubTasksOnly filterScope" <|
            \() ->
                { exampleBoardConfigForm | filterScope = "SubTasksOnly" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filterScope
                    |> Expect.equal (Ok <| Filter.SubTasksOnly)
        , test "decodes the Both filterScope" <|
            \() ->
                { exampleBoardConfigForm | filterScope = "Both" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filterScope
                    |> Expect.equal (Ok <| Filter.Both)
        , test "decodes an unrecognised filterScope as Both" <|
            \() ->
                { exampleBoardConfigForm | filterScope = "xxxxxxxxx" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.filterScope
                    |> Expect.equal (Ok <| Filter.Both)
        , test "decodes the board name" <|
            \() ->
                { exampleBoardConfigForm | name = "the board name" }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.name
                    |> Expect.equal (Ok "the board name")
        , test "trims whitespace from the board name" <|
            \() ->
                { exampleBoardConfigForm | name = "   the board name  " }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.name
                    |> Expect.equal (Ok "the board name")
        , test "allows an empty board name" <|
            \() ->
                { exampleBoardConfigForm | name = "   " }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.name
                    |> Expect.equal (Ok "")
        , test "decodes a True showColumnTags" <|
            \() ->
                { exampleBoardConfigForm | showColumnTags = True }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.showColumnTags
                    |> Expect.equal (Ok <| True)
        , test "decodes a False showColumnTags" <|
            \() ->
                { exampleBoardConfigForm | showColumnTags = False }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.showColumnTags
                    |> Expect.equal (Ok <| False)
        , test "decodes a True showFilteredTags" <|
            \() ->
                { exampleBoardConfigForm | showFilteredTags = True }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.showFilteredTags
                    |> Expect.equal (Ok <| True)
        , test "decodes a False showFilteredTags" <|
            \() ->
                { exampleBoardConfigForm | showFilteredTags = False }
                    |> SD.run BoardConfigForm.safeDecoder
                    |> Result.map BoardConfig.showFilteredTags
                    |> Expect.equal (Ok <| False)
        ]



-- HELPERS


exampleBoardConfigForm : BoardConfigForm
exampleBoardConfigForm =
    { columns = ColumnsForm.init <| Columns.fromList []
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = ""
    , showColumnTags = False
    , showFilteredTags = False
    }
