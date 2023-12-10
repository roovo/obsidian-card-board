module Column.UntaggedTests exposing (suite)

import Column
import Column.Untagged as UntaggedColumn exposing (UntaggedColumn)
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
        , updateName
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "Places an incomplete task item with no tags and no sub-tasks" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "Places an incomplete task item with no tags and incomplete sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "Places an incomplete task item with no tags and completed sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DoesNotBelong an incomplete task item with a tag which has no sub-tasks" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo #foo")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with no tags that has a tagged sub-tasks" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #bar")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "CompletedInThisColumn a completed task item with no tags and no sub-tasks" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "CompletedInThisColumn a completed task with no tags and incomplete sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "CompletedInThisColumn a completed task item with no tags and completed sub-tasks with no tags" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [x] bar")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with a tag which has no sub-tasks" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo #foo")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DoesNotBelong a completed task item with no tags that has a tagged sub-tasks" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar #bar")
                    |> Tuple.mapFirst UntaggedColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map UntaggedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map UntaggedColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map UntaggedColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map UntaggedColumn.tagsToHide
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
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample UntaggedColumn.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


init : Test
init =
    describe "init"
        [ test "initializes with no cards" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.toList
                    |> List.length
                    |> Expect.equal 0
        , test "initializes with no tagsToHide" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.tagsToHide
                    |> Expect.equal []
        , test "sets the column name" <|
            \() ->
                UntaggedColumn.init "A Column Name"
                    |> UntaggedColumn.name
                    |> Expect.equal "A Column Name"
        , test "is not collapsed" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.isCollapsed
                    |> Expect.equal False
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map (UntaggedColumn.setCollapse True)
                    |> Result.map UntaggedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map (UntaggedColumn.setCollapse True)
                    |> Result.map UntaggedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map (UntaggedColumn.setCollapse False)
                    |> Result.map UntaggedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map (UntaggedColumn.setCollapse False)
                    |> Result.map UntaggedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        ]


setNameToDefault : Test
setNameToDefault =
    describe "setNameToDefault"
        [ test "updates the name" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.setNameToDefault exampleColumnNames
                    |> UntaggedColumn.name
                    |> Expect.equal "No Tags"
        ]


setTagsToHide : Test
setTagsToHide =
    describe "setTagsToHide"
        [ test "sets the tags" <|
            \() ->
                UntaggedColumn.init ""
                    |> UntaggedColumn.setTagsToHide [ "tag 1", "tag 2" ]
                    |> UntaggedColumn.tagsToHide
                    |> Expect.equal [ "tag 1", "tag 2" ]
        ]


toList : Test
toList =
    describe "toList"
        [ test "sorts by due date and title (not case sensitive)" <|
            \() ->
                UntaggedColumn.init ""
                    |> justAdd (taskItem "- [ ] f @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] d @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] E @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] c @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] a @due(2022-01-03)")
                    |> justAdd (taskItem "- [ ] B @due(2022-01-03)")
                    |> UntaggedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "toggles a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map UntaggedColumn.toggleCollapse
                    |> Result.map UntaggedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder UntaggedColumn.decoder
                    |> .decoded
                    |> Result.map UntaggedColumn.toggleCollapse
                    |> Result.map UntaggedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name" <|
            \() ->
                UntaggedColumn.init "A Column Name"
                    |> UntaggedColumn.updateName "new name"
                    |> UntaggedColumn.name
                    |> Expect.equal "new name"
        ]



-- HELPERS


exampleColumnNames : DefaultColumnNames
exampleColumnNames =
    """{"today":"This Day","tomorrow":"The Morrow","future":"Way Out","undated":"No Date","otherTags":"Other Tags","untagged":"No Tags","completed":"Is Done"}"""
        |> DecodeHelpers.runDecoder DefaultColumnNames.v_0_11_0_decoder
        |> .decoded
        |> Result.withDefault DefaultColumnNames.default


justAdd : TaskItem -> UntaggedColumn -> UntaggedColumn
justAdd item column =
    column
        |> UntaggedColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
