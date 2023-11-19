module Column.OtherTagsTests exposing (suite)

import Column
import Column.OtherTags as OtherTagsColumn exposing (OtherTagsColumn)
import ColumnNames exposing (ColumnNames)
import Expect
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import TagBoardConfig exposing (TagBoardConfig)
import TagList
import TaskItem exposing (TaskItem)
import Test exposing (..)


suite : Test
suite =
    concat
        [ addTaskItem
        , asColumn
        , init
        , isEnabled
        , name
        ]


addTaskItem : Test
addTaskItem =
    describe "addTaskItem"
        [ describe "with nothing completed"
            [ describe "with no tag columns specified"
                [ test "Placed (Q: _none_) - [ ] foo #atag" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [ "foo" ], Column.Placed )
                , test "Placed (Q: _none_) - [ ] foo\n  - [ ] bar #atag" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [ "foo" ], Column.Placed )
                , test "DoesNotBelong (Q: _none_) - [ ] foo #atag (if includeOthers is False)" <|
                    \() ->
                        OtherTagsColumn.init
                            { defaultTagBoardConfig | includeOthers = False }
                            defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.DoesNotBelong )
                , test "DoesNotBelong (Q: _none_) - [ ] foo" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.DoesNotBelong )
                , test "DoesNotBelong (Q: _none_) - [ ] foo\n  - [ ] bar" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.DoesNotBelong )
                ]
            , describe "with a tag column specified"
                [ test "Placed (Q: xtag) - [ ] foo #atag" <|
                    \() ->
                        OtherTagsColumn.init
                            { defaultTagBoardConfig
                                | columns = [ { tag = "xtag", displayTitle = "" } ]
                            }
                            defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [ "foo" ], Column.Placed )
                , test "Placed (Q: xtag) - [ ] foo\n  - [ ] bar #atag" <|
                    \() ->
                        OtherTagsColumn.init
                            { defaultTagBoardConfig
                                | columns = [ { tag = "xtag", displayTitle = "" } ]
                            }
                            defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [ "foo" ], Column.Placed )
                , test "Placed (Q: xtag) - [ ] foo\n  - [ ] bar #atag\n  - [ ] baz #xtag" <|
                    \() ->
                        OtherTagsColumn.init
                            { defaultTagBoardConfig
                                | columns = [ { tag = "xtag", displayTitle = "" } ]
                            }
                            defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag\n  - [ ] baz #xtag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [ "foo" ], Column.Placed )
                , test "DoesNotBelong (Q: xtag) - [ ] foo #xtag" <|
                    \() ->
                        OtherTagsColumn.init
                            { defaultTagBoardConfig
                                | columns = [ { tag = "xtag", displayTitle = "" } ]
                            }
                            defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo #xtag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.DoesNotBelong )
                , test "DoesNotBelong (Q: xtag) - [ ] foo\n  - [ ] bar #xtag" <|
                    \() ->
                        OtherTagsColumn.init
                            { defaultTagBoardConfig
                                | columns = [ { tag = "xtag", displayTitle = "" } ]
                            }
                            defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #xtag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.DoesNotBelong )
                ]
            ]
        , describe "with completed tasks"
            [ describe "with no tag columns specified"
                [ test "CompletedInThisColumn (Q: _none_) - [x] foo #atag" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.CompletedInThisColumn )
                , test "CompletedInThisColumn (Q: _none_) - [x] foo\n  - [ ] bar #atag" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.CompletedInThisColumn )
                , test "CompletedInThisColumn (Q: _none_) - [ ] foo\n  - [x] bar #atag" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.CompletedInThisColumn )
                , test "Placed (Q: _none_) - [ ] foo\n  - [x] bar #atag\n  - [ ] baz #btag" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag\n  - [ ] baz #btag")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [ "foo" ], Column.Placed )
                , test "DoesNotBelong (Q: _none_) - [x] foo" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.DoesNotBelong )
                , test "DoesNotBelong (Q: _none_) - [x] foo\n  - [ ] bar" <|
                    \() ->
                        OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                            |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar")
                            |> Tuple.mapFirst OtherTagsColumn.asColumn
                            |> Tuple.mapFirst Column.items
                            |> Tuple.mapFirst (List.map TaskItem.title)
                            |> Expect.equal ( [], Column.DoesNotBelong )
                ]
            , describe "with a tag column specified"
                [ describe "with the top level task completed"
                    [ test "CompletedInThisColumn (Q: xtag) - [x] foo #atag" <|
                        \() ->
                            OtherTagsColumn.init
                                { defaultTagBoardConfig
                                    | columns = [ { tag = "xtag", displayTitle = "" } ]
                                }
                                defaultColumnNames
                                |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo #atag")
                                |> Tuple.mapFirst OtherTagsColumn.asColumn
                                |> Tuple.mapFirst Column.items
                                |> Tuple.mapFirst (List.map TaskItem.title)
                                |> Expect.equal ( [], Column.CompletedInThisColumn )
                    , test "CompletedInThisColumn (Q: xtag) - [x] foo\n  - [ ] bar #atag" <|
                        \() ->
                            OtherTagsColumn.init
                                { defaultTagBoardConfig
                                    | columns = [ { tag = "xtag", displayTitle = "" } ]
                                }
                                defaultColumnNames
                                |> OtherTagsColumn.addTaskItem (taskItem "- [x] foo\n  - [ ] bar #atag")
                                |> Tuple.mapFirst OtherTagsColumn.asColumn
                                |> Tuple.mapFirst Column.items
                                |> Tuple.mapFirst (List.map TaskItem.title)
                                |> Expect.equal ( [], Column.CompletedInThisColumn )
                    ]
                , describe "with only subtasks completed"
                    [ test "CompletedInThisColumn (Q: xtag) - [ ] foo\n  - [x] bar #atag" <|
                        \() ->
                            OtherTagsColumn.init
                                { defaultTagBoardConfig
                                    | columns = [ { tag = "xtag", displayTitle = "" } ]
                                }
                                defaultColumnNames
                                |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag")
                                |> Tuple.mapFirst OtherTagsColumn.asColumn
                                |> Tuple.mapFirst Column.items
                                |> Tuple.mapFirst (List.map TaskItem.title)
                                |> Expect.equal ( [], Column.CompletedInThisColumn )
                    , test "Placed (Q: xtag) - [ ] foo\n  - [x] bar #atag\n  - [ ] baz #btag" <|
                        \() ->
                            OtherTagsColumn.init
                                { defaultTagBoardConfig
                                    | columns = [ { tag = "xtag", displayTitle = "" } ]
                                }
                                defaultColumnNames
                                |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #atag\n  - [ ] baz #btag")
                                |> Tuple.mapFirst OtherTagsColumn.asColumn
                                |> Tuple.mapFirst Column.items
                                |> Tuple.mapFirst (List.map TaskItem.title)
                                |> Expect.equal ( [ "foo" ], Column.Placed )
                    , test "Placed (Q: xtag) - [ ] foo\n  - [ ] bar #atag\n  - [x] baz #xtag" <|
                        \() ->
                            OtherTagsColumn.init
                                { defaultTagBoardConfig
                                    | columns = [ { tag = "xtag", displayTitle = "" } ]
                                }
                                defaultColumnNames
                                |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [ ] bar #atag\n  - [x] baz #xtag")
                                |> Tuple.mapFirst OtherTagsColumn.asColumn
                                |> Tuple.mapFirst Column.items
                                |> Tuple.mapFirst (List.map TaskItem.title)
                                |> Expect.equal ( [ "foo" ], Column.Placed )
                    , test "DoesNotBelong (Q: xtag) - [ ] foo\n  - [x] bar #xtag" <|
                        \() ->
                            OtherTagsColumn.init
                                { defaultTagBoardConfig
                                    | columns = [ { tag = "xtag", displayTitle = "" } ]
                                }
                                defaultColumnNames
                                |> OtherTagsColumn.addTaskItem (taskItem "- [ ] foo\n  - [x] bar #xtag")
                                |> Tuple.mapFirst OtherTagsColumn.asColumn
                                |> Tuple.mapFirst Column.items
                                |> Tuple.mapFirst (List.map TaskItem.title)
                                |> Expect.equal ( [], Column.DoesNotBelong )
                    ]
                ]
            ]
        ]


asColumn : Test
asColumn =
    describe "asColumn"
        [ test "sorts the Column TaskItems by due date then (case insensitive) title" <|
            \() ->
                OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                    |> justAdd (taskItem "- [ ] f #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] d #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] E #atag @due(2022-01-01)")
                    |> justAdd (taskItem "- [ ] c #atag @due(2022-01-02)")
                    |> justAdd (taskItem "- [ ] a #atag @due(2022-01-03)")
                    |> justAdd (taskItem "- [ ] B #atag @due(2022-01-03)")
                    |> OtherTagsColumn.asColumn
                    |> Column.items
                    |> List.map TaskItem.title
                    |> Expect.equal [ "E", "f", "c", "d", "a", "B" ]
        ]


init : Test
init =
    describe "init"
        [ test "initializes with an empty TaskList" <|
            \() ->
                OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                    |> OtherTagsColumn.asColumn
                    |> Column.isEmpty
                    |> Expect.equal True
        ]


isEnabled : Test
isEnabled =
    describe "asColumn.isEnabled"
        [ test "returns True if the config.includeOthers is True" <|
            \() ->
                OtherTagsColumn.init { defaultTagBoardConfig | includeOthers = True } defaultColumnNames
                    |> OtherTagsColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal True
        , test "returns False if the config.includeOthers is False" <|
            \() ->
                OtherTagsColumn.init { defaultTagBoardConfig | includeOthers = False } defaultColumnNames
                    |> OtherTagsColumn.asColumn
                    |> Column.isEnabled
                    |> Expect.equal False
        ]


name : Test
name =
    describe "asColumn.name"
        [ test "defaults to 'Others'" <|
            \() ->
                OtherTagsColumn.init defaultTagBoardConfig defaultColumnNames
                    |> OtherTagsColumn.asColumn
                    |> Column.name
                    |> Expect.equal "Others"
        , test "can be customized" <|
            \() ->
                OtherTagsColumn.init defaultTagBoardConfig { defaultColumnNames | others = Just "foo" }
                    |> OtherTagsColumn.asColumn
                    |> Column.name
                    |> Expect.equal "foo"
        ]



-- HELPERS


defaultColumnNames : ColumnNames
defaultColumnNames =
    ColumnNames.default


defaultTagBoardConfig : TagBoardConfig
defaultTagBoardConfig =
    let
        default : TagBoardConfig
        default =
            TagBoardConfig.default
    in
    { default | includeOthers = True }


justAdd : TaskItem -> OtherTagsColumn -> OtherTagsColumn
justAdd item column =
    column
        |> OtherTagsColumn.addTaskItem item
        |> Tuple.first


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
