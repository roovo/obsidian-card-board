module Column.OtherTagsTests exposing (suite)

import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
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
        , otherTags
        , setCollapse
        , setNameToDefault
        , setOtherTags
        , setTagsToHide
        , toList
        , toggleCollapse
        , updateName
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ describe "incomplete tasks"
            [ test "places a tagged card when NO otherTags have been configured" <|
                \() ->
                    OtherTagsColumn.init "" []
                        |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "places a tagged card when NON-MATCHING otherTags have been configured" <|
                \() ->
                    OtherTagsColumn.init "" [ "aTa", "bTag", "aTagger", "aTag/subtag" ]
                        |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DOES NOT place an UN-tagged card when no otherTags have been configured" <|
                \() ->
                    OtherTagsColumn.init "" []
                        |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "DOES NOT place a tagged card when otherTags INCLUDES the current one" <|
                \() ->
                    OtherTagsColumn.init "" [ "aTa", "bTag", "aTagger", "aTag/subtag", "aTag" ]
                        |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "DOES NOT place a tagged card when included in another column as a sub-tag" <|
                \() ->
                    OtherTagsColumn.init "" [ "aTag/" ]
                        |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag/foo")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        , describe "completed tasks"
            [ test "CompletedInThisColumn a tagged card when no otherTags have been configured" <|
                \() ->
                    OtherTagsColumn.init "" []
                        |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #aTag")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
            , test "CompletedInThisColumn a tagged card when otherTags which don't include the current one have been configured" <|
                \() ->
                    OtherTagsColumn.init "" [ "aTa", "bTag", "aTagger", "aTag/subtag" ]
                        |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #aTag")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
            , test "CompletedInThisColumn a completed tagged card with incomplete tagged subtasks" <|
                \() ->
                    OtherTagsColumn.init "" []
                        |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #aTag\n  - [ ] bar #bTag")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
            , test "DoesNotBelong a tagged card when otherTags INCLUDES the current one" <|
                \() ->
                    OtherTagsColumn.init "" [ "aTa", "aTag", "bTag", "aTagger", "aTag/subtag", "aTag" ]
                        |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #aTag")
                        |> Tuple.mapFirst OtherTagsColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.tagsToHide
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
                        """{"collapsed":false,"name":"a name"}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample OtherTagsColumn.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


init : Test
init =
    describe "init"
        [ test "initializes with no cards" <|
            \() ->
                OtherTagsColumn.init "" []
                    |> OtherTagsColumn.toList
                    |> List.length
                    |> Expect.equal 0
        , test "initializes with no tagsToHide" <|
            \() ->
                OtherTagsColumn.init "" []
                    |> OtherTagsColumn.tagsToHide
                    |> Expect.equal []
        , test "sets the column name" <|
            \() ->
                OtherTagsColumn.init "A Column Name" []
                    |> OtherTagsColumn.name
                    |> Expect.equal "A Column Name"
        , test "is not collapsed" <|
            \() ->
                OtherTagsColumn.init "A Column Name" []
                    |> OtherTagsColumn.isCollapsed
                    |> Expect.equal False
        ]


otherTags : Test
otherTags =
    describe "otherTags"
        [ test "returns the other tags" <|
            \() ->
                OtherTagsColumn.init "" [ "aTag" ]
                    |> OtherTagsColumn.otherTags
                    |> Expect.equal [ "aTag" ]
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (OtherTagsColumn.setCollapse True)
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (OtherTagsColumn.setCollapse True)
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (OtherTagsColumn.setCollapse False)
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (OtherTagsColumn.setCollapse False)
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok False)
        ]


setNameToDefault : Test
setNameToDefault =
    describe "setNameToDefault"
        [ test "updates the name" <|
            \() ->
                OtherTagsColumn.init "A Column Name" []
                    |> OtherTagsColumn.setNameToDefault exampleColumnNames
                    |> OtherTagsColumn.name
                    |> Expect.equal "Other Tags"
        ]


setOtherTags : Test
setOtherTags =
    describe "setOtherTags"
        [ test "doesn't place TaskItem if other tags include the tag" <|
            \() ->
                OtherTagsColumn.init "" [ "aTag" ]
                    |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag")
                    |> Tuple.mapFirst OtherTagsColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "places TaskItem if I use setOtherTags to change other tags to something else" <|
            \() ->
                OtherTagsColumn.init "" [ "aTag" ]
                    |> OtherTagsColumn.setOtherTags [ "bTag" ]
                    |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag")
                    |> Tuple.mapFirst OtherTagsColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        ]


setTagsToHide : Test
setTagsToHide =
    describe "setTagsToHide"
        [ test "sets the tags" <|
            \() ->
                OtherTagsColumn.init "" []
                    |> OtherTagsColumn.setTagsToHide [ "tag 1", "tag 2" ]
                    |> OtherTagsColumn.tagsToHide
                    |> Expect.equal [ "tag 1", "tag 2" ]
        ]


toList : Test
toList =
    describe "toList"
        [ test "sorts by due date and title (not case sensitive)" <|
            \() ->
                OtherTagsColumn.init "" []
                    |> justAdd (taskItem "- [ ] f #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] d #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] E #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] c #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] a #atag @due(2022-01-03)")
                    |> justAdd (taskItem "- [ ] B #atag @due(2022-01-03)")
                    |> OtherTagsColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "toggles a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.toggleCollapse
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.toggleCollapse
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name" <|
            \() ->
                OtherTagsColumn.init "A Column Name" []
                    |> OtherTagsColumn.updateName "new name"
                    |> OtherTagsColumn.name
                    |> Expect.equal "new name"
        ]



-- HELPERS


exampleColumnNames : DefaultColumnNames
exampleColumnNames =
    """{"today":"This Day","tomorrow":"The Morrow","future":"Way Out","undated":"No Date","otherTags":"Other Tags","untagged":"No Tags","completed":"Is Done"}"""
        |> DecodeHelpers.runDecoder DefaultColumnNames.v_0_11_0_decoder
        |> .decoded
        |> Result.withDefault DefaultColumnNames.default


justAdd : TaskItem -> OtherTagsColumn -> OtherTagsColumn
justAdd item column =
    column
        |> OtherTagsColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
