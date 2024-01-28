module ColumnTests exposing (suite)

import Card
import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Column.OtherTags as OtherTagsColumn
import Date exposing (Date)
import DefaultColumnNames exposing (DefaultColumnNames)
import Expect
import Helpers.DateTimeHelpers as DateTimeHelpers
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
        , asCompletedColumn
        , cardCount
        , cards
        , containsTask
        , decoder
        , encoder
        , isCompleted
        , namedTagTag
        , setCollapse
        , setNameToDefault
        , toggleCollapse
        , typeString
        , updateOtherTags
        , updateName
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "does NOT add to a Completed Column" <|
            \() ->
                Column.completed (CompletedColumn.init "" 0 10)
                    |> Column.addTaskItem today (taskItem "- [x] foo")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "adds to a Dated Column" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 0))
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DOES NOT add an undated TaskItem to a Dated Column" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "adds to a NamedTag Column" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.addTaskItem today (taskItem "- [ ] foo #aTag")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DOES NOT add a TaskItem with a different tag to a NamedTag Column" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.addTaskItem today (taskItem "- [ ] foo #bTag")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "adds to an OtherTags Column" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.addTaskItem today (taskItem "- [ ] foo #bTag")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DOES NOT add a TaskItem with a matching tag to an OtherTags Column" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.addTaskItem today (taskItem "- [ ] foo #aTag")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "adds to an Undated Column" <|
            \() ->
                Column.undated ""
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DOES NOT add a dated TaskItem to an Undated Column" <|
            \() ->
                Column.undated ""
                    |> Column.addTaskItem today (taskItem "- [ ] foo @due(2023-01-10)")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "adds to an Untagged Column" <|
            \() ->
                Column.untagged ""
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DOES NOT add a tagged TaskItem to an Untagged Column" <|
            \() ->
                Column.untagged ""
                    |> Column.addTaskItem today (taskItem "- [ ] foo #aTag")
                    |> Tuple.mapFirst (Column.cards "")
                    |> Tuple.mapFirst (List.map Card.taskItem)
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        ]


asCompletedColumn : Test
asCompletedColumn =
    describe "asCompletedColumn"
        [ test "returns Just the CompletedColumn if it is one" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.asCompletedColumn
                    |> Maybe.map CompletedColumn.name
                    |> Expect.equal (Just "foo")
        , test "returns Nothing if it is a DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.asCompletedColumn
                    |> Expect.equal Nothing
        , test "returns Nothing if it is a NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.asCompletedColumn
                    |> Expect.equal Nothing
        , test "returns Nothing if it is a OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.asCompletedColumn
                    |> Expect.equal Nothing
        , test "returns Nothing if it is a UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.asCompletedColumn
                    |> Expect.equal Nothing
        , test "returns Nothing if it is a UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.asCompletedColumn
                    |> Expect.equal Nothing
        ]


cardCount : Test
cardCount =
    describe "cardCount"
        [ test "is zero for a column which has had no taskItems added" <|
            \() ->
                Column.undated ""
                    |> Column.cardCount
                    |> Expect.equal 0
        , test "is 1 for a column which has had one taskItem added" <|
            \() ->
                Column.undated ""
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.first
                    |> Column.cardCount
                    |> Expect.equal 1
        , test "is 3 for a column which has had three taskItems added" <|
            \() ->
                Column.undated ""
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.first
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.first
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.first
                    |> Column.cardCount
                    |> Expect.equal 3
        ]


cards : Test
cards =
    describe "cards"
        [ test "is empty for a column which has had no taskItems added" <|
            \() ->
                Column.undated ""
                    |> Column.cards ""
                    |> Expect.equal []
        , test "returns an array of cards with an id starting with the given prefix" <|
            \() ->
                Column.undated ""
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.first
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.first
                    |> Column.addTaskItem today (taskItem "- [ ] foo")
                    |> Tuple.first
                    |> Column.cards "bar"
                    |> List.map Card.id
                    |> List.map (String.left 3)
                    |> Expect.equal [ "bar", "bar", "bar" ]
        ]


containsTask : Test
containsTask =
    describe "containsTask"
        [ test "returns False if there are no taskItems" <|
            \() ->
                Column.undated ""
                    |> Column.containsTask ""
                    |> Expect.equal False
        , test "returns False if there is a non-matching taskItem" <|
            \() ->
                Column.undated ""
                    |> Column.addTaskItem today (taskItem "- [] foo")
                    |> Tuple.first
                    |> Column.containsTask "non-matching-id"
                    |> Expect.equal False
        , test "returns True if there is a matching taskItem" <|
            \() ->
                let
                    matchingTaskItem : TaskItem
                    matchingTaskItem =
                        taskItem "- [] foo"
                in
                Column.undated ""
                    |> Column.addTaskItem today matchingTaskItem
                    |> Tuple.first
                    |> Column.containsTask (TaskItem.id matchingTaskItem)
                    |> Expect.equal True
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes a CompletedColumn" <|
            \() ->
                """{"tag":"completed","data":{"collapsed":true,"index":7,"limit":5,"name":"a name"}}"""
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map Column.name
                    |> Expect.equal (Ok "a name")
        , test "decodes a DatedColumn" <|
            \() ->
                """{"tag":"dated","data":{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}}"""
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map Column.name
                    |> Expect.equal (Ok "a name")
        , test "decodes a NamedTagColumn" <|
            \() ->
                """{"tag":"namedTag","data":{"collapsed":true,"name":"a name","tag":"aTag"}}"""
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map Column.name
                    |> Expect.equal (Ok "a name")
        , test "decodes an OtherTagsColumn" <|
            \() ->
                """{"tag":"otherTags","data":{"collapsed":true,"name":"a name"}}"""
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map Column.name
                    |> Expect.equal (Ok "a name")
        , test "decodes an UndatedColumn" <|
            \() ->
                """{"tag":"undated","data":{"collapsed":true,"name":"a name"}}"""
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map Column.name
                    |> Expect.equal (Ok "a name")
        , test "decodes an UntaggedColumn" <|
            \() ->
                """{"tag":"untagged","data":{"collapsed":true,"name":"a name"}}"""
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map Column.name
                    |> Expect.equal (Ok "a name")
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a decoded CompletedColumn" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"tag":"completed","data":{"collapsed":true,"index":7,"limit":5,"name":"a name"}}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample Column.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        , test "encodes a decoded DatedColumn" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"tag":"dated","data":{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample Column.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        , test "encodes a decoded NamedTagColumn" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"tag":"namedTag","data":{"collapsed":true,"name":"a name","tag":"aTag"}}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample Column.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        , test "encodes a decoded OtherTagsColumn" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"tag":"otherTags","data":{"collapsed":true,"name":"a name"}}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample Column.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        , test "encodes a decoded UndatedColumn" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"tag":"undated","data":{"collapsed":true,"name":"a name"}}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample Column.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        , test "encodes a decoded UntaggedColumn" <|
            \() ->
                let
                    encodedString : String
                    encodedString =
                        """{"tag":"untagged","data":{"collapsed":true,"name":"a name"}}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder Column.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample Column.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


isCompleted : Test
isCompleted =
    describe "isCompleted"
        [ test "returns True if it is one" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.isCompleted
                    |> Expect.equal True
        , test "returns False if it is a DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.isCompleted
                    |> Expect.equal False
        , test "returns False if it is a NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.isCompleted
                    |> Expect.equal False
        , test "returns False if it is a OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.isCompleted
                    |> Expect.equal False
        , test "returns False if it is a UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.isCompleted
                    |> Expect.equal False
        , test "returns False if it is a UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.isCompleted
                    |> Expect.equal False
        ]


namedTagTag : Test
namedTagTag =
    describe "namedTagTag"
        [ test "returns Nothing if it is a CompletedColumn" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.namedTagTag
                    |> Expect.equal Nothing
        , test "returns Nothing if it is a DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.namedTagTag
                    |> Expect.equal Nothing
        , test "returns Just the tag name if it is a NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.namedTagTag
                    |> Expect.equal (Just "aTag")
        , test "returns Nothing if it is a OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.namedTagTag
                    |> Expect.equal Nothing
        , test "returns Nothing if it is a UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.namedTagTag
                    |> Expect.equal Nothing
        , test "returns Nothing if it is a UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.namedTagTag
                    |> Expect.equal Nothing
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "can set a CompletedColumn to be collapsed" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.setCollapse False
                    |> Column.setCollapse True
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can set a CompletedColumn to be uncollapsed" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.setCollapse True
                    |> Column.setCollapse False
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can set a DatedColumn to be collapsed" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.setCollapse False
                    |> Column.setCollapse True
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can set a DatedColumn to be uncollapsed" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.setCollapse True
                    |> Column.setCollapse False
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can set a NamedTagColumn to be collapsed" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.setCollapse False
                    |> Column.setCollapse True
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can set a NamedTagColumn to be uncollapsed" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.setCollapse True
                    |> Column.setCollapse False
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can set an OtherTagsColumn to be collapsed" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.setCollapse False
                    |> Column.setCollapse True
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can set an OtherTagsColumn to be uncollapsed" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.setCollapse True
                    |> Column.setCollapse False
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can set an UndatedColumn to be collapsed" <|
            \() ->
                Column.undated ""
                    |> Column.setCollapse False
                    |> Column.setCollapse True
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can set an UndatedColumn to be uncollapsed" <|
            \() ->
                Column.undated ""
                    |> Column.setCollapse True
                    |> Column.setCollapse False
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can set an UntaggedColumn to be collapsed" <|
            \() ->
                Column.untagged ""
                    |> Column.setCollapse False
                    |> Column.setCollapse True
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can set an UntaggedColumn to be uncollapsed" <|
            \() ->
                Column.untagged ""
                    |> Column.setCollapse True
                    |> Column.setCollapse False
                    |> Column.isCollapsed
                    |> Expect.equal False
        ]


setNameToDefault : Test
setNameToDefault =
    describe "setNameToDefault"
        [ test "sets the name of a CompletedColumn using the defaults" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "Is Done"
        , test "sets the name of a Today DatedColumn using the defaults" <|
            \() ->
                Column.dated (DatedColumn.init "Today" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "This Day"
        , test "sets the name of a Tomorrow DatedColumn using the defaults" <|
            \() ->
                Column.dated (DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "The Morrow"
        , test "sets the name of a Future DatedColumn using the defaults" <|
            \() ->
                Column.dated (DatedColumn.init "Future" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "Way Out"
        , test "DOES NOT change the name of a NamedTagColumn" <|
            \() ->
                Column.namedTag "a name" "aTag"
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "a name"
        , test "sets the name of a OtherTagsColumn using the defaults" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "Other Tags"
        , test "sets the name of a UndatedColumn using the defaults" <|
            \() ->
                Column.undated ""
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "No Date"
        , test "sets the name of a UntaggedColumn using the defaults" <|
            \() ->
                Column.untagged ""
                    |> Column.setNameToDefault exampleColumnNames
                    |> Column.name
                    |> Expect.equal "No Tags"
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "can toggleCollapse on an uncollapsed CompletedColumn" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.setCollapse False
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can toggleCollapse on a collapsed CompletedColumn" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.setCollapse True
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can toggleCollapse on an uncollapsed DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.setCollapse False
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can toggleCollapse on a collapsed DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.setCollapse True
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can toggleCollapse on an uncollapsed NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.setCollapse False
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can toggleCollapse on a collapsed NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.setCollapse True
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can toggleCollapse on an uncollapsed OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.setCollapse False
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can toggleCollapse on a collapsed OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.setCollapse True
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can toggleCollapse on an uncollapsed UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.setCollapse False
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can toggleCollapse on a collapsed UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.setCollapse True
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "can toggleCollapse on an uncollapsed UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.setCollapse False
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "can toggleCollapse on a collapsed UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.setCollapse True
                    |> Column.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        ]


typeString : Test
typeString =
    describe "typeString"
        [ test "returns 'Completed' if it is a CompletedColumn" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.typeString
                    |> Expect.equal "Completed"
        , test "returns 'Dated' if it is a DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.typeString
                    |> Expect.equal "Dated"
        , test "returns 'Tagged' if it is a NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.typeString
                    |> Expect.equal "Tagged"
        , test "returns 'Other Tags' if it is a OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.typeString
                    |> Expect.equal "Other Tags"
        , test "returns 'Undated' if it is a UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.typeString
                    |> Expect.equal "Undated"
        , test "returns 'Untagged' if it is a UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.typeString
                    |> Expect.equal "Untagged"
        ]


updateOtherTags : Test
updateOtherTags =
    describe "updateOtherTags"
        [ test "does nothing if it is a CompletedColumn" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.updateOtherTags OtherTagsColumn.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "does nothing if it is a DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.updateOtherTags OtherTagsColumn.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "does nothing if it is a NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.updateOtherTags OtherTagsColumn.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "applies the function if it is a OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.updateOtherTags OtherTagsColumn.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal True
        , test "does nothing if it is a UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.updateOtherTags OtherTagsColumn.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        , test "does nothing if it is a UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.updateOtherTags OtherTagsColumn.toggleCollapse
                    |> Column.isCollapsed
                    |> Expect.equal False
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name of a CompletedColumn" <|
            \() ->
                Column.completed (CompletedColumn.init "foo" 0 10)
                    |> Column.updateName "eek"
                    |> Column.name
                    |> Expect.equal "eek"
        , test "updates the name of a DatedColumn" <|
            \() ->
                Column.dated (DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 }))
                    |> Column.updateName "eek"
                    |> Column.name
                    |> Expect.equal "eek"
        , test "updates the name of a NamedTagColumn" <|
            \() ->
                Column.namedTag "" "aTag"
                    |> Column.updateName "eek"
                    |> Column.name
                    |> Expect.equal "eek"
        , test "updates the name of an OtherTagsColumn" <|
            \() ->
                Column.otherTags "" [ "aTag" ]
                    |> Column.updateName "eek"
                    |> Column.name
                    |> Expect.equal "eek"
        , test "updates the name of an UndatedColumn" <|
            \() ->
                Column.undated ""
                    |> Column.updateName "eek"
                    |> Column.name
                    |> Expect.equal "eek"
        , test "updates the name of an UntaggedColumn" <|
            \() ->
                Column.untagged ""
                    |> Column.updateName "eek"
                    |> Column.name
                    |> Expect.equal "eek"
        ]



-- HELPERS


dueString : Int -> String
dueString offset =
    "@due(" ++ DateTimeHelpers.offsetDateString offset ++ ")"


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


today : Date
today =
    DateTimeHelpers.todayDate
