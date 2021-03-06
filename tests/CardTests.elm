module CardTests exposing (suite)

import Card exposing (Highlight(..))
import Expect
import Helpers.TaskHelpers as TaskHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import Set
import TaskItem exposing (TaskItem)
import Test exposing (..)
import Time


suite : Test
suite =
    concat
        [ editButtonId
        , filePath
        , fromTaskItem
        , highlight
        , id
        , markdownWithIds
        , notesId
        , subtasks
        , taskItemId
        ]


editButtonId : Test
editButtonId =
    describe "editButtonId"
        [ test "adds :editButton on to the end of the Card.id" <|
            \() ->
                taskItem
                    |> Maybe.map (Card.fromTaskItem "prefix")
                    |> Maybe.map Card.editButtonId
                    |> Expect.equal (Just <| "prefix:" ++ TaskHelpers.taskId "taskItemPath" 1 ++ ":editButton")
        ]


filePath : Test
filePath =
    describe "filePath"
        [ test "returns the filePath of the taskItem" <|
            \() ->
                taskItem
                    |> Maybe.map (Card.fromTaskItem "")
                    |> Maybe.map Card.filePath
                    |> Expect.equal (Just <| "taskItemPath")
        ]


fromTaskItem : Test
fromTaskItem =
    describe "fromTaskItem"
        [ test "prefixes the Card.id with the given prefix" <|
            \() ->
                taskItem
                    |> Maybe.map (Card.fromTaskItem "prefixed")
                    |> Maybe.map Card.id
                    |> Expect.equal (Just <| "prefixed:" ++ TaskHelpers.taskId "taskItemPath" 1)
        ]


highlight : Test
highlight =
    describe "highlight"
        [ test "returns HighlightNone for a task with no due date" <|
            \() ->
                "- [ ] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (Card.fromTaskItem "")
                    |> Result.map (Card.highlight { now = now, zone = Time.utc })
                    |> Expect.equal (Ok HighlightNone)
        , test "returns HighlightImportant for a task that is due today" <|
            \() ->
                "- [ ] foo @due(2020-01-01)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (Card.fromTaskItem "")
                    |> Result.map (Card.highlight { now = now, zone = Time.utc })
                    |> Expect.equal (Ok HighlightImportant)
        , test "returns HighlightNone for a completed task that is due today" <|
            \() ->
                "- [x] foo @due(2020-01-01)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (Card.fromTaskItem "")
                    |> Result.map (Card.highlight { now = now, zone = Time.utc })
                    |> Expect.equal (Ok HighlightNone)
        , test "returns HighlightCritical for a task that is overdue" <|
            \() ->
                "- [ ] foo @due(2019-01-01)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (Card.fromTaskItem "")
                    |> Result.map (Card.highlight { now = now, zone = Time.utc })
                    |> Expect.equal (Ok HighlightCritical)
        , test "returns HighlightNone for a completed task that is overdue" <|
            \() ->
                "- [x] foo @due(2019-01-01)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (Card.fromTaskItem "")
                    |> Result.map (Card.highlight { now = now, zone = Time.utc })
                    |> Expect.equal (Ok HighlightNone)
        , test "returns HighlightGood for a task that is due in the future" <|
            \() ->
                "- [ ] foo @due(2020-01-02)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (Card.fromTaskItem "")
                    |> Result.map (Card.highlight { now = now, zone = Time.utc })
                    |> Expect.equal (Ok HighlightGood)
        , test "returns HighlightNone for a completed task that is due in the future" <|
            \() ->
                "- [x] foo @due(2020-01-02)"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map (Card.fromTaskItem "")
                    |> Result.map (Card.highlight { now = now, zone = Time.utc })
                    |> Expect.equal (Ok HighlightNone)
        ]


id : Test
id =
    describe "id"
        [ test "returns the id of the taskItem with the card prefix" <|
            \() ->
                taskItem
                    |> Maybe.map (Card.fromTaskItem "the_prefix")
                    |> Maybe.map Card.id
                    |> Expect.equal (Just "the_prefix:1754873316:1")
        ]


markdownWithIds : Test
markdownWithIds =
    describe "markdownWithIds"
        [ test "extracts the taskItem title, subtasks titles, and notes with their respective ids" <|
            \() ->
                """- [ ] foo
 some note
  - [ ] bar
 more notes
  """
                    |> Parser.run (TaskItem.parser "file" Nothing (Set.fromList []) 0)
                    |> Result.toMaybe
                    |> Maybe.map (Card.fromTaskItem "prefix")
                    |> Maybe.map Card.markdownWithIds
                    |> Expect.equal
                        (Just
                            [ { id = "prefix:" ++ TaskHelpers.taskId "file" 1 ++ ":notes", markdown = "some note\nmore notes" }
                            , { id = "prefix:" ++ TaskHelpers.taskId "file" 3, markdown = "bar" }
                            , { id = "prefix:" ++ TaskHelpers.taskId "file" 1, markdown = "foo" }
                            ]
                        )
        ]


notesId : Test
notesId =
    describe "notesId"
        [ test "adds :notes on to the end of the Card.id" <|
            \() ->
                taskItem
                    |> Maybe.map (Card.fromTaskItem "a_prefix")
                    |> Maybe.map Card.notesId
                    |> Expect.equal (Just <| "a_prefix:" ++ TaskHelpers.taskId "taskItemPath" 1 ++ ":notes")
        ]


subtasks : Test
subtasks =
    describe "subtasks"
        [ test "returns an empty list if there are no subtasks" <|
            \() ->
                taskItem
                    |> Maybe.map (Card.fromTaskItem "")
                    |> Maybe.map Card.subtasks
                    |> Expect.equal (Just [])
        , test "returns a list of the subtasks with the card idPrefix" <|
            \() ->
                """- [ ] foo

  - [ ] bar"""
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.toMaybe
                    |> Maybe.map (Card.fromTaskItem "a_prefix")
                    |> Maybe.map Card.subtasks
                    |> Maybe.map (List.map <| Tuple.mapSecond TaskItem.title)
                    |> Expect.equal (Just [ ( "a_prefix:2166136261:3", "bar" ) ])
        ]


taskItemId : Test
taskItemId =
    describe "taskItemId"
        [ test "taskItemId is just the id of the taskItem (without the idPrefix)" <|
            \() ->
                taskItem
                    |> Maybe.map (Card.fromTaskItem "foo")
                    |> Maybe.map Card.taskItemId
                    |> Expect.equal (Just (TaskHelpers.taskId "taskItemPath" 1))
        ]



-- HELPERS


now : Time.Posix
now =
    -- 2020-01-01
    Time.millisToPosix 1577836800000


taskItem : Maybe TaskItem
taskItem =
    "- [ ] foo"
        |> Parser.run (TaskItem.parser "taskItemPath" Nothing (Set.fromList []) 0)
        |> Result.toMaybe
