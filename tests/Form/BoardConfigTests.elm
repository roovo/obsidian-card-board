module Form.BoardConfigTests exposing (suite)

import BoardConfig
import Column
import Column.Completed as CompletedColumn
import Columns
import DefaultColumnNames exposing (DefaultColumnNames)
import Expect
import Filter
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Form.SafeDecoder as SD
import Helpers.FilterHelpers as FilterHelpers
import Test exposing (..)


suite : Test
suite =
    concat
        [ fromNewBoardForm
        , init
        , safeDecoder
        , mapColumnsForm
        , toggleShowColumnTags
        , toggleShowFilteredTags
        , updateFilterPolarity
        , updateFilterScope
        , updateName
        ]


fromNewBoardForm : Test
fromNewBoardForm =
    describe "fromNewBoardForm"
        [ describe "emptyBoard"
            [ test "has no columns" <|
                \() ->
                    NewBoardForm "" "emptyBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .columnsForm
                        |> Expect.equal ColumnsForm.empty
            , test "has no filters" <|
                \() ->
                    NewBoardForm "" "emptyBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .filters
                        |> Expect.equal []
            , test "has Allow filterPolarity" <|
                \() ->
                    NewBoardForm "" "emptyBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .filterPolarity
                        |> Expect.equal "Allow"
            , test "has Both filterScope" <|
                \() ->
                    NewBoardForm "" "emptyBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .filterScope
                        |> Expect.equal "Both"
            , test "sets the board name" <|
                \() ->
                    NewBoardForm "foo" "emptyBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .name
                        |> Expect.equal "foo"
            , test "shows the column tags" <|
                \() ->
                    NewBoardForm "" "emptyBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .showColumnTags
                        |> Expect.equal True
            , test "shows the filtered tags" <|
                \() ->
                    NewBoardForm "" "emptyBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .showFilteredTags
                        |> Expect.equal True
            ]
        , describe "dateBoard"
            [ test "has the basic date columns" <|
                \() ->
                    NewBoardForm "" "dateBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .columnsForm
                        |> .columnForms
                        |> List.map ColumnForm.name
                        |> Expect.equal [ "Undated", "Today", "Tomorrow", "Future", "Completed" ]
            , test "can override the basic date column names" <|
                \() ->
                    NewBoardForm "" "dateBoard"
                        |> BoardConfigForm.fromNewBoardForm customColumnNames
                        |> .columnsForm
                        |> .columnForms
                        |> List.map ColumnForm.name
                        |> Expect.equal [ "No Date", "This Day", "The Morrow", "Way Out", "Done" ]
            ]
        , describe "tagBoard"
            [ test "has the basic tag board columns" <|
                \() ->
                    NewBoardForm "" "tagBoard"
                        |> BoardConfigForm.fromNewBoardForm DefaultColumnNames.default
                        |> .columnsForm
                        |> .columnForms
                        |> List.map ColumnForm.name
                        |> Expect.equal [ "Untagged", "Other Tags", "Completed" ]
            , test "can override the basic tag board column names" <|
                \() ->
                    NewBoardForm "" "tagBoard"
                        |> BoardConfigForm.fromNewBoardForm customColumnNames
                        |> .columnsForm
                        |> .columnForms
                        |> List.map ColumnForm.name
                        |> Expect.equal [ "No Tags", "Other Taggy-Waggys", "Done" ]
            ]
        ]


init : Test
init =
    describe "init"
        [ test "initialises the columns from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | columns = Columns.fromList [ Column.undated "foo", Column.untagged "bar" ] }
                    |> BoardConfigForm.init
                    |> .columnsForm
                    |> Expect.equal
                        { columnForms =
                            [ ColumnForm.UndatedColumnForm { name = "foo" }
                            , ColumnForm.UntaggedColumnForm { name = "bar" }
                            ]
                        }
        , test "initialises the filters from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | filters = FilterHelpers.exampleFilters }
                    |> BoardConfigForm.init
                    |> .filters
                    |> Expect.equal FilterHelpers.exampleFilters
        , test "initialises the Allow filterPolarity from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | filterPolarity = Filter.Allow }
                    |> BoardConfigForm.init
                    |> .filterPolarity
                    |> Expect.equal "Allow"
        , test "initialises the Deny filterPolarity from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | filterPolarity = Filter.Deny }
                    |> BoardConfigForm.init
                    |> .filterPolarity
                    |> Expect.equal "Deny"
        , test "initialises the TopLevelOnly filterScope from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | filterScope = Filter.TopLevelOnly }
                    |> BoardConfigForm.init
                    |> .filterScope
                    |> Expect.equal "TopLevelOnly"
        , test "initialises the SubTasksOnly filterScope from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | filterScope = Filter.SubTasksOnly }
                    |> BoardConfigForm.init
                    |> .filterScope
                    |> Expect.equal "SubTasksOnly"
        , test "initialises the Both filterScope from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | filterScope = Filter.Both }
                    |> BoardConfigForm.init
                    |> .filterScope
                    |> Expect.equal "Both"
        , test "initialises the name from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | name = "a name" }
                    |> BoardConfigForm.init
                    |> .name
                    |> Expect.equal "a name"
        , test "initialises True showColumnTags from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | showColumnTags = True }
                    |> BoardConfigForm.init
                    |> .showColumnTags
                    |> Expect.equal True
        , test "initialises False showColumnTags from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | showColumnTags = False }
                    |> BoardConfigForm.init
                    |> .showColumnTags
                    |> Expect.equal False
        , test "initialises True showFilteredTags from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | showFilteredTags = True }
                    |> BoardConfigForm.init
                    |> .showFilteredTags
                    |> Expect.equal True
        , test "initialises False showFilteredTags from the board config" <|
            \() ->
                BoardConfig.fromConfig { exampleBoardConfig | showFilteredTags = False }
                    |> BoardConfigForm.init
                    |> .showFilteredTags
                    |> Expect.equal False
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes the columns" <|
            \() ->
                { exampleBoardConfigForm
                    | columnsForm =
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


mapColumnsForm : Test
mapColumnsForm =
    describe "mapColumnsForm"
        [ test "applies the map function to the ColumnsForm" <|
            \() ->
                { exampleBoardConfigForm | columnsForm = ColumnsForm.init <| Columns.fromList [ Column.untagged "foo" ] }
                    |> BoardConfigForm.mapColumnsForm (always ColumnsForm.empty)
                    |> .columnsForm
                    |> Expect.equal ColumnsForm.empty
        ]


toggleShowColumnTags : Test
toggleShowColumnTags =
    describe "toggleShowColumnTags"
        [ test "toggles showColumnTags from False to True" <|
            \() ->
                { exampleBoardConfigForm | showColumnTags = False }
                    |> BoardConfigForm.toggleShowColumnTags
                    |> .showColumnTags
                    |> Expect.equal True
        , test "toggles showColumnTags from True to False" <|
            \() ->
                { exampleBoardConfigForm | showColumnTags = True }
                    |> BoardConfigForm.toggleShowColumnTags
                    |> .showColumnTags
                    |> Expect.equal False
        ]


toggleShowFilteredTags : Test
toggleShowFilteredTags =
    describe "toggleShowFilteredTags"
        [ test "toggles showFilteredTags from False to True" <|
            \() ->
                { exampleBoardConfigForm | showFilteredTags = False }
                    |> BoardConfigForm.toggleShowFilteredTags
                    |> .showFilteredTags
                    |> Expect.equal True
        , test "toggles showFilteredTags from True to False" <|
            \() ->
                { exampleBoardConfigForm | showFilteredTags = True }
                    |> BoardConfigForm.toggleShowFilteredTags
                    |> .showFilteredTags
                    |> Expect.equal False
        ]


updateFilterPolarity : Test
updateFilterPolarity =
    describe "updateFilterPolarity"
        [ test "sets the filterPolarity to the given value" <|
            \() ->
                { exampleBoardConfigForm | filterPolarity = "" }
                    |> BoardConfigForm.updateFilterPolarity "foo"
                    |> .filterPolarity
                    |> Expect.equal "foo"
        ]


updateFilterScope : Test
updateFilterScope =
    describe "updateFilterScope"
        [ test "sets the filterPolarity to the given value" <|
            \() ->
                { exampleBoardConfigForm | filterScope = "" }
                    |> BoardConfigForm.updateFilterScope "foo"
                    |> .filterScope
                    |> Expect.equal "foo"
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "sets the name to the given value" <|
            \() ->
                { exampleBoardConfigForm | name = "" }
                    |> BoardConfigForm.updateName "foo"
                    |> .name
                    |> Expect.equal "foo"
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


exampleBoardConfig : BoardConfig.Config
exampleBoardConfig =
    { columns = Columns.empty
    , filters = []
    , filterPolarity = Filter.Allow
    , filterScope = Filter.Both
    , name = ""
    , showColumnTags = False
    , showFilteredTags = False
    }


exampleBoardConfigForm : BoardConfigForm
exampleBoardConfigForm =
    { columnsForm = ColumnsForm.init <| Columns.fromList []
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = ""
    , showColumnTags = False
    , showFilteredTags = False
    }
