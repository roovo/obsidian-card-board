module Column.DatedTests exposing (suite)

import Column
import Column.Dated as DatedColumn exposing (DatedColumn)
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
        , decoder
        , encoder
        , init
        , setCollapse
        , setNameToDefault
        , setTagsToHide
        , toList
        , toggleCollapse
        , updateName
        , updateRangeType
        , updateRangeValueFrom
        , updateRangeValueTo
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ describe "Range is Between"
            [ test "Places an incomplete task item with a matching due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 0))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "Places an incomplete task item with a matching due date even if from and to are the wrong way around" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 2, to = 0 })
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 1))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "CompletedInThisColumn a completed task item with a matching due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                        |> DatedColumn.addTaskItem today (taskItem ("- [x] foo " ++ dueString 0))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
            , test "DoesNotBelong an incomplete task item with no due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                        |> DatedColumn.addTaskItem today (taskItem "- [ ] foo ")
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "DoesNotBelong an incomplete task item with a due date outside the bottom of the range" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString -1))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "DoesNotBelong an incomplete task item with a due date outside the top of the range" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 1))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        , describe "Range is Before"
            [ test "Places an incomplete task item with a matching due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString -1))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "CompletedInThisColumn a completed task item with a matching due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [x] foo " ++ dueString -1))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
            , test "DoesNotBelong an incomplete task item with no due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 0)
                        |> DatedColumn.addTaskItem today (taskItem "- [ ] foo ")
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "Places an incomplete task item with a due date lower in the range" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString -6))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with a due date outside the top of the range" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 0))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        , describe "Range is After"
            [ test "Places an incomplete task item with a matching due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 1))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "CompletedInThisColumn a completed task item with a matching due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [x] foo " ++ dueString 1))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.CompletedInThisColumn )
            , test "DoesNotBelong an incomplete task item with no due date" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 0)
                        |> DatedColumn.addTaskItem today (taskItem "- [ ] foo ")
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            , test "Places an incomplete task item with a due date higher in the range" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 6))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [ "foo" ], PlacementResult.Placed )
            , test "DoesNotBelong an incomplete task item with a due date outside the bvottom of the range" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 0)
                        |> DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 0))
                        |> Tuple.mapFirst DatedColumn.toList
                        |> Tuple.mapFirst (List.map TaskItem.title)
                        |> Expect.equal ( [], PlacementResult.DoesNotBelong )
            ]
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "decodes collapsed field" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map DatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "decodes name field" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map DatedColumn.name
                    |> Expect.equal (Ok "a name")
        , test "decodes After range field - accept" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"after","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 2)))
                    |> Result.map Tuple.first
                    |> Result.map DatedColumn.toList
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "decodes After range field - reject" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"after","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 1)))
                    |> Result.map Tuple.first
                    |> Result.map DatedColumn.toList
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [])
        , test "decodes Before range field - accept" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 0)))
                    |> Result.map Tuple.first
                    |> Result.map DatedColumn.toList
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "decodes Before range field - reject" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 1)))
                    |> Result.map Tuple.first
                    |> Result.map DatedColumn.toList
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [])
        , test "decodes Between range field - accept" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 5)))
                    |> Result.map Tuple.first
                    |> Result.map DatedColumn.toList
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [ "foo" ])
        , test "decodes Between range field - reject too low" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 4)))
                    |> Result.map Tuple.first
                    |> Result.map DatedColumn.toList
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [])
        , test "decodes Between range field - reject too high" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.addTaskItem today (taskItem ("- [ ] foo " ++ dueString 8)))
                    |> Result.map Tuple.first
                    |> Result.map DatedColumn.toList
                    |> Result.map (List.map TaskItem.title)
                    |> Expect.equal (Ok [])
        , test "decode result has no taskItems" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map DatedColumn.toList
                    |> Expect.equal (Ok [])
        , test "decode result has no tagsToHide" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map DatedColumn.tagsToHide
                    |> Expect.equal (Ok [])
        ]


encoder : Test
encoder =
    describe "encoder"
        [ test "encodes a decoded column" <|
            \() ->
                let
                    encodedString =
                        """{"collapsed":true,"name":"a name","range":{"tag":"between","data":{"from":5,"to":7}}}"""
                in
                encodedString
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (TsEncode.runExample DatedColumn.encoder)
                    |> Result.map .output
                    |> Expect.equal (Ok encodedString)
        ]


init : Test
init =
    describe "init"
        [ test "initializes with no cards" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.toList
                    |> List.length
                    |> Expect.equal 0
        , test "initializes with no tagsToHide" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.tagsToHide
                    |> Expect.equal []
        , test "sets the column name" <|
            \() ->
                DatedColumn.init "A Column Name" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.name
                    |> Expect.equal "A Column Name"
        , test "is not collapsed" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.isCollapsed
                    |> Expect.equal False
        ]


setCollapse : Test
setCollapse =
    describe "setCollapse"
        [ test "sets a collapsed column to be collapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.setCollapse True)
                    |> Result.map DatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.setCollapse True)
                    |> Result.map DatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        , test "sets a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.setCollapse False)
                    |> Result.map DatedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "sets an uncollapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map (DatedColumn.setCollapse False)
                    |> Result.map DatedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        ]


setNameToDefault : Test
setNameToDefault =
    describe "setNameToDefault"
        [ test "updates the name if set to Today" <|
            \() ->
                DatedColumn.init "Today" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.setNameToDefault exampleColumnNames
                    |> DatedColumn.name
                    |> Expect.equal "This Day"
        , test "updates the name if set to Tomorrow" <|
            \() ->
                DatedColumn.init "Tomorrow" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.setNameToDefault exampleColumnNames
                    |> DatedColumn.name
                    |> Expect.equal "The Morrow"
        , test "updates the name if set to Future" <|
            \() ->
                DatedColumn.init "Future" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.setNameToDefault exampleColumnNames
                    |> DatedColumn.name
                    |> Expect.equal "Way Out"
        ]


setTagsToHide : Test
setTagsToHide =
    describe "setTagsToHide"
        [ test "sets the tags" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Between { from = 0, to = 0 })
                    |> DatedColumn.setTagsToHide [ "tag 1", "tag 2" ]
                    |> DatedColumn.tagsToHide
                    |> Expect.equal [ "tag 1", "tag 2" ]
        ]


toList : Test
toList =
    describe "toList"
        [ test "sorts by due date and title (not case sensitive)" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Between { from = 0, to = 2 })
                    |> justAdd (taskItem ("- [ ] f " ++ dueString 0))
                    |> justAdd (taskItem ("- [ ] d " ++ dueString 1))
                    |> justAdd (taskItem ("- [ ] E " ++ dueString 0))
                    |> justAdd (taskItem ("- [ ] c " ++ dueString 1))
                    |> justAdd (taskItem ("- [ ] a " ++ dueString 2))
                    |> justAdd (taskItem ("- [ ] B " ++ dueString 2))
                    |> DatedColumn.toList
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


toggleCollapse : Test
toggleCollapse =
    describe "toggleCollapse"
        [ test "toggles a collapsed column to be uncollapsed" <|
            \() ->
                """{"collapsed":true,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map DatedColumn.toggleCollapse
                    |> Result.map DatedColumn.isCollapsed
                    |> Expect.equal (Ok False)
        , test "toggles an uncollapsed column to be collapsed" <|
            \() ->
                """{"collapsed":false,"name":"a name","range":{"tag":"before","data":1}}"""
                    |> DecodeHelpers.runDecoder DatedColumn.decoder
                    |> .decoded
                    |> Result.map DatedColumn.toggleCollapse
                    |> Result.map DatedColumn.isCollapsed
                    |> Expect.equal (Ok True)
        ]


updateName : Test
updateName =
    describe "updateName"
        [ test "updates the name" <|
            \() ->
                DatedColumn.init "A Column Name" (DatedColumn.Before 1)
                    |> DatedColumn.updateName "new name"
                    |> DatedColumn.name
                    |> Expect.equal "new name"
        ]


updateRangeType : Test
updateRangeType =
    describe "updateRangeType"
        [ describe "originally 'Before'"
            [ test "does nothing if being set to 'Before'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 3)
                        |> DatedColumn.updateRangeType "Before"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.Before 3)
            , test "maintains the range value if being set to 'After'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 3)
                        |> DatedColumn.updateRangeType "After"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.After 3)
            , test "maintains the range values if being set to 'Between'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Before 3)
                        |> DatedColumn.updateRangeType "Between"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.Between { from = 3, to = 3 })
            ]
        , describe "originally 'After'"
            [ test "maintains the range value if being set to 'Before'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 3)
                        |> DatedColumn.updateRangeType "Before"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.Before 3)
            , test "does nothing if being set to 'After'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 3)
                        |> DatedColumn.updateRangeType "After"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.After 3)
            , test "maintains the range values if being set to 'Between'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.After 3)
                        |> DatedColumn.updateRangeType "Between"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.Between { from = 3, to = 3 })
            ]
        , describe "originally 'Between'"
            [ test "uses the from range value if being set to 'Before'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 2, to = 3 })
                        |> DatedColumn.updateRangeType "Before"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.Before 2)
            , test "uses the to range value if being set to 'Before' and to is smaller than from" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 5, to = 3 })
                        |> DatedColumn.updateRangeType "Before"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.Before 3)
            , test "uses the to range value if being set to 'After'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 2, to = 3 })
                        |> DatedColumn.updateRangeType "After"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.After 3)
            , test "uses the from range value if being set to 'After' and it is larger than to" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 5, to = 3 })
                        |> DatedColumn.updateRangeType "After"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.After 5)
            , test "does nothing if being set to 'Between'" <|
                \() ->
                    DatedColumn.init "" (DatedColumn.Between { from = 2, to = 3 })
                        |> DatedColumn.updateRangeType "Between"
                        |> DatedColumn.range
                        |> Expect.equal (DatedColumn.Between { from = 2, to = 3 })
            ]
        ]


updateRangeValueFrom : Test
updateRangeValueFrom =
    describe "updateRangeValueFrom"
        [ test "does nothing to a Before" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Before 3)
                    |> DatedColumn.updateRangeValueFrom 7
                    |> DatedColumn.range
                    |> Expect.equal (DatedColumn.Before 3)
        , test "updates the range value of an After" <|
            \() ->
                DatedColumn.init "" (DatedColumn.After 3)
                    |> DatedColumn.updateRangeValueFrom 7
                    |> DatedColumn.range
                    |> Expect.equal (DatedColumn.After 7)
        , test "updates the from the value of a Between" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Between { from = 3, to = 4 })
                    |> DatedColumn.updateRangeValueFrom 7
                    |> DatedColumn.range
                    |> Expect.equal (DatedColumn.Between { from = 7, to = 4 })
        ]


updateRangeValueTo : Test
updateRangeValueTo =
    describe "updateRangeValueTo"
        [ test "updates the range value of a Before" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Before 3)
                    |> DatedColumn.updateRangeValueTo 7
                    |> DatedColumn.range
                    |> Expect.equal (DatedColumn.Before 7)
        , test "does nothing to an After" <|
            \() ->
                DatedColumn.init "" (DatedColumn.After 3)
                    |> DatedColumn.updateRangeValueTo 7
                    |> DatedColumn.range
                    |> Expect.equal (DatedColumn.After 3)
        , test "updates the to the value of a Between" <|
            \() ->
                DatedColumn.init "" (DatedColumn.Between { from = 3, to = 4 })
                    |> DatedColumn.updateRangeValueTo 7
                    |> DatedColumn.range
                    |> Expect.equal (DatedColumn.Between { from = 3, to = 7 })
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


today : Date
today =
    DateTimeHelpers.todayDate


justAdd : TaskItem -> DatedColumn -> DatedColumn
justAdd item column =
    column
        |> DatedColumn.addTaskItem today item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
