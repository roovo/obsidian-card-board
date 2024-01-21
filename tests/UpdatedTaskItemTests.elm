module UpdatedTaskItemTests exposing (suite)

import DataviewTaskCompletion
import Date
import Expect
import Fuzz
import GlobalSettings
import Helpers.TaskItemHelpers as TaskItemHelpers
import Maybe.Extra as ME
import Parser
import Session exposing (TaskCompletionSettings)
import TaskItem
import Test exposing (..)
import Time
import TimeWithZone exposing (TimeWithZone)
import UpdatedTaskItem


suite : Test
suite =
    concat
        [ completionString
        , dueString
        , toString
        , toggleCompletion
        , updateDate
        ]


completionString : Test
completionString =
    describe "completionString"
        [ fuzz3 (Fuzz.intRange -1440 1440) Fuzz.bool Fuzz.bool "returns an empty string if the format is NoCompletion" <|
            \zone inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone zone [] }
                    |> UpdatedTaskItem.completionString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.NoCompletion
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal ""
        , test "returns a utc completion string with no UTC offset if the format is ObsidianCardBoard" <|
            \() ->
                { time = Time.millisToPosix 0, zone = Time.customZone 0 [] }
                    |> UpdatedTaskItem.completionString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.ObsidianCardBoard
                        , inLocalTime = False
                        , showUtcOffset = False
                        }
                    |> Expect.equal "@completed(1970-01-01T00:00:00)"
        , test "returns a local completion string with an UTC offset if the format is ObsidianCardBoard" <|
            \() ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> UpdatedTaskItem.completionString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.ObsidianCardBoard
                        , inLocalTime = True
                        , showUtcOffset = True
                        }
                    |> Expect.equal "@completed(1970-01-01T05:00:00+05:00)"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is ObsidianTasks" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> UpdatedTaskItem.completionString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.ObsidianTasks
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "✅ 1970-01-01"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is the default for ObsidianDataview" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> UpdatedTaskItem.completionString
                        { dataviewTaskCompletion = DataviewTaskCompletion.default
                        , format = GlobalSettings.ObsidianDataview
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "[completion:: 1970-01-01]"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is a specified ObsidianDataview" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> UpdatedTaskItem.completionString
                        { dataviewTaskCompletion = DataviewTaskCompletion.Text "done"
                        , format = GlobalSettings.ObsidianDataview
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "[done:: 1970-01-01]"
        , fuzz2 Fuzz.bool Fuzz.bool "returns a completion string if the format is ObsidianDataview.Emoji" <|
            \inLocalTime showUtcOffset ->
                { time = Time.millisToPosix 0, zone = Time.customZone 300 [] }
                    |> UpdatedTaskItem.completionString
                        { dataviewTaskCompletion = DataviewTaskCompletion.Emoji
                        , format = GlobalSettings.ObsidianDataview
                        , inLocalTime = inLocalTime
                        , showUtcOffset = showUtcOffset
                        }
                    |> Expect.equal "✅ 1970-01-01"
        ]


dueString : Test
dueString =
    describe "dueString"
        [ test "returns a CardBoard format due date for NoCompletion format" <|
            \() ->
                Date.fromRataDie 738906
                    |> UpdatedTaskItem.dueString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.NoCompletion
                        , inLocalTime = False
                        , showUtcOffset = False
                        }
                    |> Expect.equal "@due(2024-01-21)"
        , test "returns a CardBoard format due date for CardBoard format" <|
            \() ->
                Date.fromRataDie 738906
                    |> UpdatedTaskItem.dueString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.ObsidianCardBoard
                        , inLocalTime = False
                        , showUtcOffset = False
                        }
                    |> Expect.equal "@due(2024-01-21)"
        , test "returns a Tasks format due date for ObsidianTasks format" <|
            \() ->
                Date.fromRataDie 738906
                    |> UpdatedTaskItem.dueString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.ObsidianTasks
                        , inLocalTime = False
                        , showUtcOffset = False
                        }
                    |> Expect.equal "📅 2024-01-21"
        , test "returns a DataView format due date for ObsidianDataview format" <|
            \() ->
                Date.fromRataDie 738906
                    |> UpdatedTaskItem.dueString
                        { dataviewTaskCompletion = DataviewTaskCompletion.NoCompletion
                        , format = GlobalSettings.ObsidianDataview
                        , inLocalTime = False
                        , showUtcOffset = False
                        }
                    |> Expect.equal "[due:: 2024-01-21]"
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
                , test "preserves leading whitespace of subtasks" <|
                    \() ->
                        "- [ ] foo\n   - [ ] bar #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.toMaybe
                            |> Maybe.map TaskItem.descendantTasks
                            |> Maybe.map List.head
                            |> ME.join
                            |> Maybe.map UpdatedTaskItem.init
                            |> Maybe.map (UpdatedTaskItem.toggleCompletion noCompletionSettings timeAtEpoc)
                            |> Maybe.map UpdatedTaskItem.toString
                            |> Expect.equal (Just "   - [x] bar #tag1 bar #tag2 ^12345")
                ]
            , describe "ObsidianCardBoard format"
                [ test "outputs a completed task string (with a UTC CardBoard formatted completed tag)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion cardBoardCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [x] foo #tag1 bar #tag2 @completed(1970-01-01T00:00:00) ^12345")
                , test "returns the original text if completion is toggled twice" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.toggleCompletion cardBoardCompletionSettings timeAtEpoc)
                            |> Result.map (UpdatedTaskItem.toggleCompletion cardBoardCompletionSettings timeAtEpoc)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
                , test "outputs a completed task string (with a local time CardBoard formatted completed tag)" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.toggleCompletion
                                    { cardBoardCompletionSettings | inLocalTime = True, showUtcOffset = True }
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
            , test "preserves leading whitespace of subtasks wich use a * marker" <|
                \() ->
                    "- [ ] foo\n   * [x] @completed(2020-03-22T00:00:00Z) bar #tag1 bar #tag2 ^12345"
                        |> Parser.run TaskItemHelpers.basicParser
                        |> Result.toMaybe
                        |> Maybe.map TaskItem.descendantTasks
                        |> Maybe.map List.head
                        |> ME.join
                        |> Maybe.map UpdatedTaskItem.init
                        |> Maybe.map (UpdatedTaskItem.toggleCompletion noCompletionSettings timeAtEpoc)
                        |> Maybe.map UpdatedTaskItem.toString
                        |> Expect.equal (Just "   * [ ] bar #tag1 bar #tag2 ^12345")
            ]
        ]


updateDate : Test
updateDate =
    describe "updateDate"
        [ describe "No file or inline date"
            [ describe "NoCompletion format"
                [ test "outputs the original text if the new date is Nothing" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.updateDate noCompletionSettings Nothing)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
                , test "outputs the task item with the given due date if present" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.updateDate
                                    noCompletionSettings
                                    (Date.fromIsoString "2024-01-21" |> Result.toMaybe)
                                )
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 @due(2024-01-21) ^12345")
                ]
            , describe "CardBoard format"
                [ test "outputs the original text if the new date is Nothing" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.updateDate cardBoardCompletionSettings Nothing)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
                , test "outputs the task item with the given due date if present" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.updateDate
                                    cardBoardCompletionSettings
                                    (Date.fromIsoString "2024-01-21" |> Result.toMaybe)
                                )
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 @due(2024-01-21) ^12345")
                ]
            , describe "ObsidianTasks format"
                [ test "outputs the original text if the new date is Nothing" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.updateDate tasksCompletionSettings Nothing)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
                , test "outputs the task item with the given due date if present" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.updateDate
                                    tasksCompletionSettings
                                    (Date.fromIsoString "2024-01-21" |> Result.toMaybe)
                                )
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 📅 2024-01-21 ^12345")
                ]
            , describe "ObsidianDataview format"
                [ test "outputs the original text if the new date is Nothing" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map (UpdatedTaskItem.updateDate dataviewCompletionSettings Nothing)
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 ^12345")
                , test "outputs the task item with the given due date if present" <|
                    \() ->
                        "- [ ] foo #tag1 bar #tag2 ^12345"
                            |> Parser.run TaskItemHelpers.basicParser
                            |> Result.map UpdatedTaskItem.init
                            |> Result.map
                                (UpdatedTaskItem.updateDate
                                    dataviewCompletionSettings
                                    (Date.fromIsoString "2024-01-21" |> Result.toMaybe)
                                )
                            |> Result.map UpdatedTaskItem.toString
                            |> Expect.equal (Ok "- [ ] foo #tag1 bar #tag2 [due:: 2024-01-21] ^12345")
                ]
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


cardBoardCompletionSettings : TaskCompletionSettings
cardBoardCompletionSettings =
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
