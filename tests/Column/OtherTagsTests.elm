module Column.OtherTagsTests exposing (suite)

import Column
import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import DefaultColumnNames exposing (DefaultColumnNames)
import Expect
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import PlacementResult
import TagBoardConfig exposing (TagBoardConfig)
import TaskItem exposing (TaskItem)
import Test exposing (..)
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ addTaskItem
        , decoder
        , disable
        , enable
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
        [ test "places an incomplete tagged card when no otherTags have been configured" <|
            \() ->
                OtherTagsColumn.init "" [ "aTa", "bTag", "aTagger", "aTag/subtag" ]
                    |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag")
                    |> Tuple.mapFirst OtherTagsColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DOES NOT place an incomplete tagged card when otherTags INCLUDES the current one" <|
            \() ->
                OtherTagsColumn.init "" [ "aTa", "bTag", "aTagger", "aTag/subtag", "aTag" ]
                    |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #aTag")
                    |> Tuple.mapFirst OtherTagsColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DOES NOT place a completed tagged card when no otherTags have been configured" <|
            \() ->
                OtherTagsColumn.init "" []
                    |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #aTag")
                    |> Tuple.mapFirst OtherTagsColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "DOES NOT place a completed tagged card when otherTags which don't include the current one have been configured" <|
            \() ->
                OtherTagsColumn.init "" [ "aTa", "bTag", "aTagger", "aTag/subtag" ]
                    |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #aTag")
                    |> Tuple.mapFirst OtherTagsColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "DOES NOT place a completed tagged card when otherTags INCLUDES the current one" <|
            \() ->
                OtherTagsColumn.init "" [ "aTa", "aTag", "bTag", "aTagger", "aTag/subtag", "aTag" ]
                    |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #aTag")
                    |> Tuple.mapFirst OtherTagsColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"enabled":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes enabled field" <|
            \() ->
                """{"collapsed":true,"enabled":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.isEnabled
                    |> Expect.equal (Ok False)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"enabled":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"enabled":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"enabled":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.tagsToHide
                    |> Expect.equal (Ok [])
        ]


disable : Test
disable =
    describe "disable"
        [ test "disables an enabled Column" <|
            \() ->
                """{"collapsed":true,"enabled":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.disable
                    |> Result.map OtherTagsColumn.isEnabled
                    |> Expect.equal (Ok False)
        , test "disables a disabled Column" <|
            \() ->
                """{"collapsed":true,"enabled":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.disable
                    |> Result.map OtherTagsColumn.isEnabled
                    |> Expect.equal (Ok False)
        ]


enable : Test
enable =
    describe "enable"
        [ test "enables an enabled Column" <|
            \() ->
                """{"collapsed":true,"enabled":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.enable
                    |> Result.map OtherTagsColumn.isEnabled
                    |> Expect.equal (Ok True)
        , test "enables a disabled Column" <|
            \() ->
                """{"collapsed":true,"enabled":false,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.enable
                    |> Result.map OtherTagsColumn.isEnabled
                    |> Expect.equal (Ok True)
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a decoded column" <|
            \() ->
                let
                    encodedString =
                        """{"collapsed":false,"enabled":true,"name":"a name"}"""
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
        , test "is enabled" <|
            \() ->
                OtherTagsColumn.init "A Column Name" []
                    |> OtherTagsColumn.isEnabled
                    |> Expect.equal True
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"enabled":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (OtherTagsColumn.setCollapse True)
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"enabled":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (OtherTagsColumn.setCollapse True)
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"enabled":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map (OtherTagsColumn.setCollapse False)
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"enabled":true,"name":"a name"}"""
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
                """{"collapsed":true,"enabled":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.toggleCollapse
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"enabled":true,"name":"a name"}"""
                    |> DecodeHelpers.runDecoder OtherTagsColumn.decoder
                    |> .decoded
                    |> Result.map OtherTagsColumn.toggleCollapse
                    |> Result.map OtherTagsColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]



-- HELPERS


exampleColumnNames : DefaultColumnNames
exampleColumnNames =
    """{"today":"This Day","tomorrow":"The Morrow","future":"Way Out","undated":"No Date","others":"Other Tags","untagged":"No Tags","completed":"Is Done"}"""
        |> DecodeHelpers.runDecoder DefaultColumnNames.decoder
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
