module UpdatedTaskItemTests exposing (suite)

import DataviewTaskCompletion
import Expect
import GlobalSettings
import Helpers.TaskItemHelpers as TaskItemHelpers
import Parser
import Session exposing (TaskCompletionSettings)
import Test exposing (..)
import Time
import TimeWithZone exposing (TimeWithZone)
import UpdatedTaskItem


suite : Test
suite =
    concat
        [ toString
        , toggleCompletion
        ]


toString : Test
toString =
    describe "toString"
        [ test "returns the task's original text if it has not been updated" <|
            \() ->
                "- [x] foo"
                    |> Parser.run TaskItemHelpers.basicParser
                    |> Result.map UpdatedTaskItem.init
                    |> Result.map UpdatedTaskItem.toString
                    |> Expect.equal (Ok "- [x] foo")
        ]


toggleCompletion : Test
toggleCompletion =
    describe "toggleCompletion"
        [ describe "incomplete tasks"
            [ describe "NoCompletion format"
                [ test "outputs a completed task string (with no completed tag)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion noCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ^12345")
                , test "returns the original text if completion is toggled twice" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion noCompletionSettings timeAtEpoc)
                            |> Result.map (UpdatedTaskItem.toggleCompletion noCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
                ]
            , describe "ObsidianCardBoard format"
                [ test "outputs a completed task string (with a UTC CardBoard formatted completed tag)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion obsidianCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 @completed(1970-01-01T00:00:00) ^12345")
                , test "returns the original text if completion is toggled twice" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion obsidianCompletionSettings timeAtEpoc)
                            |> Result.map (UpdatedTaskItem.toggleCompletion obsidianCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
                , test "outputs a completed task string (with a local time CardBoard formatted completed tag)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.toggleCompletion
                                    { obsidianCompletionSettings | inLocalTime = True, showUtcOffset = True }
                                    { timeAtEpoc | zone = Time.customZone 300 [] }
                                )
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 @completed(1970-01-01T05:00:00+05:00) ^12345")
                ]
            , describe "ObsidianTasks format"
                [ test "outputs a completed task string (with a Tasks formatted completed tag)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion tasksCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ✅ 1970-01-01 ^12345")
                ]
            , describe "ObsidianDataview format"
                [ test "outputs a completed task string (respects DataView NoCompletion)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion dataviewCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ^12345")
                , test "outputs a completed task string (respects DataView custom completion string)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.toggleCompletion
                                    { dataviewCompletionSettings | dataviewTaskCompletion = DataviewTaskCompletion.Text "done" }
                                    timeAtEpoc
                                )
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 [done:: 1970-01-01] ^12345")
                , test "outputs a completed task string (respects DataView emoji completion string)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.toggleCompletion
                                    { dataviewCompletionSettings | dataviewTaskCompletion = DataviewTaskCompletion.Emoji }
                                    timeAtEpoc
                                )
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 ✅ 1970-01-01 ^12345")
                ]
            ]
        , describe "completed tasks"
            [ test "removes all completed timestamps respecting the dataview completion format" <|
                \() ->
                    "- [x] foo #tag1 bar #tag2 @completed(2020-03-22T00:00:00Z) @completed(2020-03-22T00:00:00-01:00) @completed(2020-03-22T00:00:00+01:00) @completed(2020-03-22T00:00:00) ✅ 1970-01-01 [done:: 2021-01-01] ^12345"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map UpdatedTaskItem.init
                        |> Result.map
                            (UpdatedTaskItem.toggleCompletion
                                { dataviewCompletionSettings | dataviewTaskCompletion = DataviewTaskCompletion.Text "done" }
                                timeAtEpoc
                            )
                        |> Result.map UpdatedTaskItem.toString
                        |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
            , test "handles a [X] checkbox" <|
                \() ->
                    "- [X] foo #tag1 bar #tag2 @completed(2020-03-22T00:00:00Z) @completed(2020-03-22T00:00:00-01:00) @completed(2020-03-22T00:00:00+01:00) @completed(2020-03-22T00:00:00) ✅ 1970-01-01 [done:: 2021-01-01] ^12345"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map UpdatedTaskItem.init
                        |> Result.map
                            (UpdatedTaskItem.toggleCompletion
                                { dataviewCompletionSettings | dataviewTaskCompletion = DataviewTaskCompletion.Text "done" }
                                timeAtEpoc
                            )
                        |> Result.map UpdatedTaskItem.toString
                        |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
            , test "handles a + checkbox marker" <|
                \() ->
                    "+ [X] foo #tag1 bar #tag2 @completed(2020-03-22T00:00:00Z) @completed(2020-03-22T00:00:00-01:00) @completed(2020-03-22T00:00:00+01:00) @completed(2020-03-22T00:00:00) ✅ 1970-01-01 [done:: 2021-01-01] ^12345"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map UpdatedTaskItem.init
                        |> Result.map
                            (UpdatedTaskItem.toggleCompletion
                                { dataviewCompletionSettings | dataviewTaskCompletion = DataviewTaskCompletion.Text "done" }
                                timeAtEpoc
                            )
                        |> Result.map UpdatedTaskItem.toString
                        |> Expect.equal (Ok "+ [ ] foo #tag1 bar #tag2 ^12345")
            , test "removes all completed timestamps respecting the dataview completion format of none" <|
                \() ->
                    "- [x] foo #tag1 bar #tag2 @completed(2020-03-22T00:00:00Z) @completed(2020-03-22T00:00:00-01:00) @completed(2020-03-22T00:00:00+01:00) @completed(2020-03-22T00:00:00) ✅ 1970-01-01 [done:: 2021-01-01] ^12345"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.map UpdatedTaskItem.init
                        |> Result.map
                            (UpdatedTaskItem.toggleCompletion
                                dataviewCompletionSettings
                                timeAtEpoc
                            )
                        |> Result.map UpdatedTaskItem.toString
                        |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 [done:: 2021-01-01] ^12345")
            ]
        ]



-- HELPERS


dataviewCompletionSettings : TaskCompletionSettings
dataviewCompletionSettings =
    { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
    , format = GlobalSettings.ObsidianDataview
    , inLocalTime = False
    , showUtcOffset = False
    }


noCompletionSettings : TaskCompletionSettings
noCompletionSettings =
    { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
    , format = GlobalSettings.NoCompletion
    , inLocalTime = False
    , showUtcOffset = False
    }


obsidianCompletionSettings : TaskCompletionSettings
obsidianCompletionSettings =
    { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
    , format = GlobalSettings.ObsidianCardBoard
    , inLocalTime = False
    , showUtcOffset = False
    }


tasksCompletionSettings : TaskCompletionSettings
tasksCompletionSettings =
    { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
    , format = GlobalSettings.ObsidianTasks
    , inLocalTime = False
    , showUtcOffset = False
    }


timeAtEpoc : TimeWithZone
timeAtEpoc =
    { time = Time.millisToPosix 0, zone = Time.customZone 0 [] }



--
--
--
--
--
--
-- , test "preserves leading whitespace for descendant tasks" <|
--     \() ->
--         "- [X] the task\n   \t- [ ] a subtask"
--             |> Parser.run TaskItemHelpers.basicParser
--             |> Result.map TaskItem.descendantTasks
--             |> Result.withDefault []
--             |> List.map
--                 (TaskItem.toggleCompletion
--                     DataviewTaskCompletion.NoCompletion
--                     { format = GlobalSettings.ObsidianCardBoard
--                     , inLocalTime = False
--                     , showUtcOffset = False
--                     }
--                     { time = Time.millisToPosix 0, zone = Time.customZone 0 [] }
--                 )
--             |> Expect.equal [ "   \t- [x] a subtask @completed(1970-01-01T00:00:00)" ]
-- , test "preserves leading whitepace" <|
--     \() ->
--         "- [ ] foo\n   - [ ] bar #tag1 bar #tag2 ^12345"
--             |> Parser.run TaskItemHelpers.basicParser
--             |> Result.toMaybe
--             |> Maybe.map TaskItem.descendantTasks
--             |> Maybe.map List.head
--             |> ME.join
--             |> Maybe.map
--                 (TaskItem.toggleCompletion
--                     DataviewTaskCompletion.NoCompletion
--                     { format = GlobalSettings.NoCompletion
--                     , inLocalTime = False
--                     , showUtcOffset = False
--                     }
--                     { time = Time.millisToPosix 0, zone = Time.customZone 0 [] }
--                 )
--             |> Expect.equal (Just "   - [x] bar #tag1 bar #tag2 ^12345")
-- , test "preserves leading whitepace with a '*' list marker" <|
--     \() ->
--         "- [ ] foo\n   * [ ] bar #tag1 bar #tag2 ^12345"
--             |> Parser.run TaskItemHelpers.basicParser
--             |> Result.toMaybe
--             |> Maybe.map TaskItem.descendantTasks
--             |> Maybe.map List.head
--             |> ME.join
--             |> Maybe.map
--                 (TaskItem.toggleCompletion
--                     DataviewTaskCompletion.NoCompletion
--                     { format = GlobalSettings.NoCompletion
--                     , inLocalTime = False
--                     , showUtcOffset = False
--                     }
--                     { time = Time.millisToPosix 0, zone = Time.customZone 0 [] }
--                 )
--             |> Expect.equal (Just "   * [x] bar #tag1 bar #tag2 ^12345")
