module Column.UndatedTests exposing (suite)

import Column
import Column.Undated as UndatedColumn exposing (UndatedColumn)
import DateBoardConfig exposing (DateBoardConfig)
import DefaultColumnNames exposing (DefaultColumnNames)
import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import PlacementResult
import TaskItem exposing (TaskItem)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ addTaskItem
        , decoder
        , encoder
        , init
        , setCollapse
        , setNameToDefault
        , setTagsToHide
        , toList
        , toggleCollapse
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "Places an incomplete task item with no due date" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst UndatedColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "Places an incomplete task item with no tags and incomplete sub-tasks with no due date" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UndatedColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "Places an incomplete task item with no tags and completed sub-tasks with no due date" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar")
                    |> Tuple.mapFirst UndatedColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DoesNotBelong an incomplete task item with a due date" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo @due(2022-01-01)")
                    |> Tuple.mapFirst UndatedColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "Places an incomplete task item with no due date that has a sub-task with a due date" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar @due(2022-01-01)")
                    |> Tuple.mapFirst UndatedColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "CompletedInThisColumn a completed task item with no due date" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.addTaskItem (taskItem "- [x] foo")
                    |> Tuple.mapFirst UndatedColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with a a due date" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.addTaskItem (taskItem "- [x] foo @due(2022-01-01)")
                    |> Tuple.mapFirst UndatedColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map UndatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map UndatedColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map UndatedColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map UndatedColumn.tagsToHide
                    |> Expect.equal (Ok [])
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a decoded column" <|
            \() ->
                let
                    encodedString =
                        """{"collapsed":false,"name":"a name"}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample UndatedColumn.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


init : Test
init =
    describe "init"
        [ test "initializes with no cards" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.toList
                    |> List.length
                    |> Expect.equal 0
        , test "initializes with no tagsToHide" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.tagsToHide
                    |> Expect.equal []
        , test "sets the column name" <|
            \() ->
                UndatedColumn.init "A Column Name"
                    |> UndatedColumn.name
                    |> Expect.equal "A Column Name"
        , test "is not collapsed" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.isCollapsed
                    |> Expect.equal False
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map (UndatedColumn.setCollapse True)
                    |> Result.map UndatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map (UndatedColumn.setCollapse True)
                    |> Result.map UndatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map (UndatedColumn.setCollapse False)
                    |> Result.map UndatedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map (UndatedColumn.setCollapse False)
                    |> Result.map UndatedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        ]


setNameToDefault : Test
setNameToDefault =
    describe "setNameToDefault"
        [ test "updates the name" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.setNameToDefault exampleColumnNames
                    |> UndatedColumn.name
                    |> Expect.equal "No Date"
        ]


setTagsToHide : Test
setTagsToHide =
    describe "setTagsToHide"
        [ test "sets the tags" <|
            \() ->
                UndatedColumn.init ""
                    |> UndatedColumn.setTagsToHide [ "tag 1", "tag 2" ]
                    |> UndatedColumn.tagsToHide
                    |> Expect.equal [ "tag 1", "tag 2" ]
        ]


toList : Test
toList =
    describe "toList"
        [ test "sorts by title (not case sensitive)" <|
            \() ->
                UndatedColumn.init ""
                    |> justAdd (taskItem "- [ ] f")
                    |> justAdd (taskItem "- [ ] d")
                    |> justAdd (taskItem "- [ ] E")
                    |> justAdd (taskItem "- [ ] c")
                    |> justAdd (taskItem "- [ ] a")
                    |> justAdd (taskItem "- [ ] B")
                    |> UndatedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B", "c", "d", "E", "f" ]
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "toggles a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map UndatedColumn.toggleCollapse
                    |> Result.map UndatedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UndatedColumn.decoder
                    |> .decoded
                    |> Result.map UndatedColumn.toggleCollapse
                    |> Result.map UndatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]



-- HELPERS


exampleColumnNames : DefaultColumnNames
exampleColumnNames =
    """{"today":"This Day","tomorrow":"The Morrow","future":"Way Out","undated":"No Date","others":"Other Tags","untagged":"No Tags","completed":"Is Done"}"""
        |> DecodeHelpers.runDecoder DefaultColumnNames.decoder
        |> .decoded
        |> Result.withDefault DefaultColumnNames.default


justAdd : TaskItem -> UndatedColumn -> UndatedColumn
justAdd item column =
    column
        |> UndatedColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
