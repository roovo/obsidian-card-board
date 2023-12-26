module ColumnsTests exposing (suite)

import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Columns exposing (Columns)
import DefaultColumnNames
import Expect
import Form.NewColumnConfig exposing (NewColumnConfigForm)
import Test exposing (..)


suite : Test
suite =
    concat
        [ cleanupNames
        , fromList
        , restrictSpecialColumns
        ]


cleanupNames : Test
cleanupNames =
    describe "cleanupNames"
        [ test "does nothing if there are no Columns" <|
            \() ->
                Columns.empty
                    |> Columns.cleanupNames DefaultColumnNames.default
                    |> Columns.toList
                    |> Expect.equal []
        , test "replaces an empty standard named columns with their default names" <|
            \() ->
                [ Column.completed <| CompletedColumn.init "" 1 10
                , Column.otherTags "" []
                , Column.undated ""
                , Column.untagged ""
                ]
                    |> Columns.fromList
                    |> Columns.cleanupNames DefaultColumnNames.default
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "Completed", "Other Tags", "Undated", "Untagged" ]
        , test "replaces an empty date and tag columns with Unnamed" <|
            \() ->
                [ Column.dated <| DatedColumn.init "  " (DatedColumn.Before 1)
                , Column.namedTag " " "aTag"
                , Column.dated <| DatedColumn.init "" (DatedColumn.After 1)
                , Column.namedTag "" "bTag"
                ]
                    |> Columns.fromList
                    |> Columns.cleanupNames DefaultColumnNames.default
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "Unnamed", "Unnamed.1", "Unnamed.2", "Unnamed.3" ]
        , test "appends the index to re-used named" <|
            \() ->
                [ Column.completed <| CompletedColumn.init "eek" 1 10
                , Column.otherTags "" []
                , Column.undated "eek"
                , Column.untagged "eek"
                , Column.namedTag "eek" "aTag"
                , Column.dated <| DatedColumn.init "eek" (DatedColumn.After 1)
                ]
                    |> Columns.fromList
                    |> Columns.cleanupNames DefaultColumnNames.default
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "eek", "Other Tags", "eek.2", "eek.3", "eek.4", "eek.5" ]
        ]


fromList : Test
fromList =
    describe "fromList"
        [ test "works with an empty list" <|
            \() ->
                []
                    |> Columns.fromList
                    |> Columns.toList
                    |> Expect.equal []
        , test "works with a list containing a single column" <|
            \() ->
                [ Column.otherTags "others" [] ]
                    |> Columns.fromList
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "others" ]
        , test "preserves columns order" <|
            \() ->
                [ Column.otherTags "others" []
                , Column.undated "undated"
                , Column.dated <| DatedColumn.init "today" (DatedColumn.Before 1)
                , Column.untagged "untagged"
                , Column.completed <| CompletedColumn.init "completed" 1 10
                , Column.namedTag "tagged" "aTag"
                ]
                    |> Columns.fromList
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "others", "undated", "today", "untagged", "completed", "tagged" ]
        ]


restrictSpecialColumns : Test
restrictSpecialColumns =
    describe "restrictSpecialColumns"
        [ test "does nothing if there are no Columns" <|
            \() ->
                Columns.empty
                    |> Columns.restrictSpecialColumns
                    |> Columns.toList
                    |> Expect.equal []
        , test "does nothing if there are multiple date and tag columns" <|
            \() ->
                [ Column.namedTag "one" "aTag"
                , Column.namedTag "two" "bTag"
                , Column.dated <| DatedColumn.init "three" (DatedColumn.After 1)
                , Column.dated <| DatedColumn.init "four" (DatedColumn.After 1)
                ]
                    |> Columns.fromList
                    |> Columns.restrictSpecialColumns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "one", "two", "three", "four" ]
        , test "keeps the first if there are multiple special columns" <|
            \() ->
                [ Column.completed <| CompletedColumn.init "1" 1 10
                , Column.completed <| CompletedColumn.init "2" 1 10
                , Column.otherTags "3" []
                , Column.otherTags "4" []
                , Column.undated "5"
                , Column.undated "6"
                , Column.untagged "7"
                , Column.untagged "8"
                ]
                    |> Columns.fromList
                    |> Columns.restrictSpecialColumns
                    |> Columns.toList
                    |> List.map Column.name
                    |> Expect.equal [ "1", "3", "5", "7" ]
        ]
