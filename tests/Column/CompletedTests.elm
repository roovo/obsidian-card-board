module Column.CompletedTests exposing (suite)

import Column.Completed as CompletedColumn
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
        , updateCompletedCount
        , updateName
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "does not add a task item if there are no PlacementResults" <|
            \() ->
                CompletedColumn.init "" 0 10
                    |> CompletedColumn.addTaskItem
                        []
                        (taskItem "- [x] foo")
                    |> CompletedColumn.toList
                    |> Expect.equal []
        , test "adds the task item if all the PlacementResults are CompletedInThisColumn" <|
            \() ->
                CompletedColumn.init "" 0 10
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn, PlacementResult.CompletedInThisColumn, PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [x] foo")
                    |> CompletedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds an INCOMPELTE task item if all the PlacementResults are CompletedInThisColumn" <|
            \() ->
                CompletedColumn.init "" 0 10
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn, PlacementResult.CompletedInThisColumn, PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] foo")
                    |> CompletedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "adds the task item if one of the PlacementResults is DoesNotBelong" <|
            \() ->
                CompletedColumn.init "" 0 10
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn, PlacementResult.DoesNotBelong, PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [x] foo")
                    |> CompletedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "foo" ]
        , test "does not add the task item if one of the PlacementResults is Placed" <|
            \() ->
                CompletedColumn.init "" 0 10
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn, PlacementResult.Placed, PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [x] foo")
                    |> CompletedColumn.toList
                    |> Expect.equal []
        , test "does not add the task item if all the PlacementResults are Placed" <|
            \() ->
                CompletedColumn.init "" 0 10
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.Placed, PlacementResult.Placed, PlacementResult.Placed ]
                        (taskItem "- [x] foo")
                    |> CompletedColumn.toList
                    |> Expect.equal []
        , test "does not add the task item if all the PlacementResults are DoesNotBelong" <|
            \() ->
                CompletedColumn.init "" 0 10
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.DoesNotBelong, PlacementResult.DoesNotBelong, PlacementResult.DoesNotBelong ]
                        (taskItem "- [x] foo")
                    |> CompletedColumn.toList
                    |> Expect.equal []
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes index field" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.index
                    |> Expect.equal (Ok 7)
        , test "decodes limit field" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.limit
                    |> Expect.equal (Ok 5)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.tagsToHide
                    |> Expect.equal (Ok [])
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a decoded column" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample CompletedColumn.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


init : Test
init =
    describe "init"
        [ test "initializes with no cards" <|
            \() ->
                CompletedColumn.init "" 2 3
                    |> CompletedColumn.toList
                    |> List.length
                    |> Expect.equal 0
        , test "initializes with no tagsToHide" <|
            \() ->
                CompletedColumn.init "" 2 3
                    |> CompletedColumn.tagsToHide
                    |> Expect.equal []
        , test "sets the column name" <|
            \() ->
                CompletedColumn.init "A Column Name" 2 3
                    |> CompletedColumn.name
                    |> Expect.equal "A Column Name"
        , test "sets the index" <|
            \() ->
                CompletedColumn.init "A Column Name" 2 3
                    |> CompletedColumn.index
                    |> Expect.equal 2
        , test "sets the limit" <|
            \() ->
                CompletedColumn.init "A Column Name" 2 3
                    |> CompletedColumn.limit
                    |> Expect.equal 3
        , test "is not collapsed" <|
            \() ->
                CompletedColumn.init "" 2 3
                    |> CompletedColumn.isCollapsed
                    |> Expect.equal False
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map (CompletedColumn.setCollapse True)
                    |> Result.map CompletedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map (CompletedColumn.setCollapse True)
                    |> Result.map CompletedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map (CompletedColumn.setCollapse False)
                    |> Result.map CompletedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map (CompletedColumn.setCollapse False)
                    |> Result.map CompletedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        ]


setNameToDefault : Test
setNameToDefault =
    describe "setNameToDefault"
        [ test "updates the name" <|
            \() ->
                CompletedColumn.init "A Column Name" 2 3
                    |> CompletedColumn.setNameToDefault exampleColumnNames
                    |> CompletedColumn.name
                    |> Expect.equal "Is Done"
        ]


setTagsToHide : Test
setTagsToHide =
    describe "setTagsToHide"
        [ test "sets the tags" <|
            \() ->
                CompletedColumn.init "" 1 2
                    |> CompletedColumn.setTagsToHide [ "tag 1", "tag 2" ]
                    |> CompletedColumn.tagsToHide
                    |> Expect.equal [ "tag 1", "tag 2" ]
        ]


toList : Test
toList =
    describe "toList"
        [ test "sorts by completed time (descending) and title (not case sensitive)" <|
            \() ->
                CompletedColumn.init "" 1 10
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] f @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] d @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] E @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] c @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] a @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] B @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B", "c", "d", "E", "f" ]
        , test "limits the number as per the config" <|
            \() ->
                CompletedColumn.init "" 1 2
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] f @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] d @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] E @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] c @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] a @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] B @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B" ]
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "toggles a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.toggleCollapse
                    |> Result.map CompletedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"index":7,"limit":5,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder CompletedColumn.decoder
                    |> .decoded
                    |> Result.map CompletedColumn.toggleCollapse
                    |> Result.map CompletedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]


updateCompletedCount : Test
updateCompletedCount =
    describe "updateCompletedCount"
        [ test "limits the number as per the updated completed count" <|
            \() ->
                CompletedColumn.init "" 1 2
                    |> CompletedColumn.updateCompletedCount 3
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] f @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] d @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] E @completed(2022-01-01T00:00:02)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] c @completed(2022-01-02T00:00:01)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] a @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.addTaskItem
                        [ PlacementResult.CompletedInThisColumn ]
                        (taskItem "- [ ] B @completed(2022-01-03T00:00:00)")
                    |> CompletedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "a", "B", "c" ]
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name" <|
            \() ->
                CompletedColumn.init "A Column Name" 2 3
                    |> CompletedColumn.updateName "new name"
                    |> CompletedColumn.name
                    |> Expect.equal "new name"
        ]



-- HELPERS


exampleColumnNames : DefaultColumnNames
exampleColumnNames =
    """{"today":"This Day","tomorrow":"The Morrow","future":"Way Out","undated":"No Date","otherTags":"Other Tags","untagged":"No Tags","completed":"Is Done"}"""
        |> DecodeHelpers.runDecoder DefaultColumnNames.v_0_11_0_decoder
        |> .decoded
        |> Result.withDefault DefaultColumnNames.default


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
