module Column.NamedTagTests exposing (suite)

import Column.NamedTag as NamedTagColumn exposing (NamedTagColumn)
import Expect
import Form.Decoder as FD
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
        , asInputString
        , decoder
        , encoder
        , init
        , setCollapse
        , setTagsToHide
        , tag
        , toList
        , toggleCollapse
        , updateName
        , updateTag
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ test "Places an incomplete task item with no sub-tasks and a matching tag" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo #atag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "Places an incomplete task item with no sub-tasks and a matching tag (amongst others)" <|
            \() ->
                NamedTagColumn.init "" "btag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "Places an incomplete task item with no tags and an incomplete sub-task with a matching tag" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "Places an incomplete task item with no tags and an incomplete sub-task with a matching tag (amongst others)" <|
            \() ->
                NamedTagColumn.init "" "btag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
        , test "DoesNotBelong an incomplete task item with no tags and no sub-tasks" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with a non-matching tag and no sub-tasks" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo #xtag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Tuple.mapFirst (List.map TaskItem.title)
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with no tags and an incomplete sub-task with a non-matching tag" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #xtag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "CompletedInThisColumn a completed task item with a matching tag and no sub-tasks" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo #atag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "CompletedInThisColumn an incomplete task item with a matching tag (amongst others) and no sub-tasks" <|
            \() ->
                NamedTagColumn.init "" "btag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "CompletedInThisColumn an incomplete task item with no tags and a completed sub-task with a matching tag" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "CompletedInThisColumn an incomplete task item with no tags and a completed sub-task with a matching tag (amongst others)" <|
            \() ->
                NamedTagColumn.init "" "btag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag #btag #ctag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
        , test "DoesNotBelong a completed task item with no tags and no sub-tasks" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DoesNotBelong a completed task item with a non-matching tag and no sub-tasks" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo #xtag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DoesNotBelong a completed task item with no tags and an incomplete sub-task with a non-matching tag" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar #xtag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        , test "DoesNotBelong an incomplete task item with no tags and a completed sub-task with a non-matching tag" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> NamedTagColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #xtag")
                    |> Tuple.mapFirst NamedTagColumn.toList
                    |> Expect.equal ( [], PlacementResult.DoesNotBelong )
        ]


asInputString : Test
asInputString =
    describe "asInputString"
        [ test "outputs '#<tag> <name>'" <|
            \() ->
                NamedTagColumn.init "a name" "atag"
                    |> NamedTagColumn.asInputString
                    |> Expect.equal "#atag a name"
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map NamedTagColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map NamedTagColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decodes tag field" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map NamedTagColumn.tag
                    |> Expect.equal (Ok "aTag")
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map NamedTagColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map NamedTagColumn.tagsToHide
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
                        """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample NamedTagColumn.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


init : Test
init =
    describe "init"
        [ test "initializes with no cards" <|
            \() ->
                NamedTagColumn.init "" ""
                    |> NamedTagColumn.toList
                    |> List.length
                    |> Expect.equal 0
        , test "initializes with no tagsToHide" <|
            \() ->
                NamedTagColumn.init "" ""
                    |> NamedTagColumn.tagsToHide
                    |> Expect.equal []
        , test "sets the column name" <|
            \() ->
                NamedTagColumn.init "A Column Name" ""
                    |> NamedTagColumn.name
                    |> Expect.equal "A Column Name"
        , test "sets the column tag" <|
            \() ->
                NamedTagColumn.init "" "aTag"
                    |> NamedTagColumn.tag
                    |> Expect.equal "aTag"
        , test "is not collapsed" <|
            \() ->
                NamedTagColumn.init "" ""
                    |> NamedTagColumn.isCollapsed
                    |> Expect.equal False
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map (NamedTagColumn.setCollapse True)
                    |> Result.map NamedTagColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map (NamedTagColumn.setCollapse True)
                    |> Result.map NamedTagColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map (NamedTagColumn.setCollapse False)
                    |> Result.map NamedTagColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map (NamedTagColumn.setCollapse False)
                    |> Result.map NamedTagColumn.isCollapsed
                    |> Expect.equal (Ok False)
        ]


setTagsToHide : Test
setTagsToHide =
    describe "setTagsToHide"
        [ test "sets the tags" <|
            \() ->
                NamedTagColumn.init "" ""
                    |> NamedTagColumn.setTagsToHide [ "tag 1", "tag 2" ]
                    |> NamedTagColumn.tagsToHide
                    |> Expect.equal [ "tag 1", "tag 2" ]
        ]


tag : Test
tag =
    describe "tag"
        [ test "returns the column's tag" <|
            \() ->
                NamedTagColumn.init "" "aTag"
                    |> NamedTagColumn.tag
                    |> Expect.equal "aTag"
        ]


toList : Test
toList =
    describe "toList"
        [ test "sorts by due date and title (not case sensitive)" <|
            \() ->
                NamedTagColumn.init "" "atag"
                    |> justAdd (taskItem "- [ ] f #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] d #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] E #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] c #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] a #atag @due(2022-01-03)")
                    |> justAdd (taskItem "- [ ] B #atag @due(2022-01-03)")
                    |> NamedTagColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "toggles a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map NamedTagColumn.toggleCollapse
                    |> Result.map NamedTagColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","tag":"aTag"}"""
                    |> DecodeHelpers.runDecoder NamedTagColumn.decoder
                    |> .decoded
                    |> Result.map NamedTagColumn.toggleCollapse
                    |> Result.map NamedTagColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name" <|
            \() ->
                NamedTagColumn.init "A Column Name" "atag"
                    |> NamedTagColumn.updateName "new name"
                    |> NamedTagColumn.name
                    |> Expect.equal "new name"
        ]


updateTag : Test
updateTag =
    describe "updateTag"
        [ test "updates the name" <|
            \() ->
                NamedTagColumn.init "A Column Name" "atag"
                    |> NamedTagColumn.updateTag "newTag"
                    |> NamedTagColumn.tag
                    |> Expect.equal "newTag"
        ]



-- HELPERS


justAdd : TaskItem -> NamedTagColumn -> NamedTagColumn
justAdd item column =
    column
        |> NamedTagColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
