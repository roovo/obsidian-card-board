module InteropDefinitionsTests exposing (suite)

import BoardConfig exposing (BoardConfig(..))
import Column
import Column.Completed as CompletedColumn
import Column.Dated as DatedColumn
import Columns
import DataviewTaskCompletion
import DefaultColumnNames
import DragAndDrop.BeaconPosition as BeaconPosition
import DragAndDrop.DragData as DragData
import Expect
import Filter
import GlobalSettings
import Helpers.DecodeHelpers as DecodeHelpers
import Helpers.FilterHelpers as FilterHelpers
import Helpers.TaskItemHelpers as TaskItemHelpers
import InteropDefinitions exposing (interop)
import Parser
import SafeZipper
import Semver
import TaskItem exposing (TaskItem, TaskItemFields)
import TaskList exposing (TaskList)
import Test exposing (..)
import TextDirection
import Time
import TsJson.Encode as TsEncode


suite : Test
suite =
    concat
        [ flagsTests
        , fromElmTests
        , toElmTests
        ]


flagsTests : Test
flagsTests =
    describe "interop.flags (decoding)"
        [ test "decodes valid flags for settings version 0.13.0" <|
            \() ->
                """{"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"firstDayOfWeek":0,"now":11,"rightToLeft":false,"uniqueId":"12345","zone":22,
                "settings":{"version":"0.13.0","data":{"globalSettings":{"defaultColumnNames":{"today":"Do Today","tomorrow":"","future":"The Future","undated":"","otherTags":"The Others","untagged":"","completed":"Completed"},"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"fileFilter","data":"aFile"}],"firstDayOfWeek":"Thu","ignoreFileNameDates":true,"taskCompletionFormat":"ObsidianTasks","taskCompletionInLocalTime":true,"taskCompletionShowUtcOffset":false},
                "boardConfigs":[
                   {"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],
                    "filterPolarity":"Deny",
                    "filterScope":"TopLevelOnly",
                    "showFilteredTags":true,
                    "showColumnTags":false,
                    "columns":[{"tag":"undated","data":{"collapsed":false,"name":"Undated"}},{"tag":"dated","data":{"collapsed":false,"name":"Do Today","range":{"tag":"before","data":1}}},{"tag":"dated","data":{"collapsed":false,"name":"Tomorrow","range":{"tag":"between","data":{"from":1,"to":1}}}},{"tag":"dated","data":{"collapsed":false,"name":"The Future","range":{"tag":"after","data":1}}},{"tag":"completed","data":{"collapsed":false,"name":"Completed","index":4,"limit":4}}],
                    "name":"date board name"},
                   {"columns":[{"tag":"untagged","data":{"collapsed":false,"name":"Untagged"}},{"tag":"namedTag","data":{"collapsed":true,"name":"name 1","tag":"tag1"}},{"tag":"completed","data":{"collapsed":false,"name":"Completed","index":2,"limit":5}}],
                    "showColumnTags":false,
                    "filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],
                    "filterPolarity":"Allow",
                    "filterScope":"SubTasksOnly",
                    "showFilteredTags":false,
                    "name":"tag board name"}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Do Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "The Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.TopLevelOnly
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board name"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "name 1" "tag1" |> Column.setCollapse True
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.SubTasksOnly
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board name"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Just "Do Today"
                                        , tomorrow = Nothing
                                        , future = Just "The Future"
                                        , undated = Nothing
                                        , otherTags = Just "The Others"
                                        , untagged = Nothing
                                        , completed = Just "Completed"
                                        }
                                    , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.fileFilter "aFile" ]
                                    , firstDayOfWeek = GlobalSettings.SpecificWeekday Time.Thu
                                    , ignoreFileNameDates = True
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = True
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.12.0" <|
            \() ->
                """{"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"firstDayOfWeek":0,"now":11,"rightToLeft":false,"uniqueId":"12345","zone":22,"settings":{"version":"0.12.0","data":{"globalSettings":{"defaultColumnNames":{"today":"Do Today","tomorrow":"","future":"The Future","undated":"","otherTags":"The Others","untagged":"","completed":"Completed"},"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"fileFilter","data":"aFile"}],"ignoreFileNameDates":true,"taskCompletionFormat":"ObsidianTasks","taskCompletionInLocalTime":true,"taskCompletionShowUtcOffset":false},
                "boardConfigs":[
                   {"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],
                    "filterPolarity":"Deny",
                    "filterScope":"TopLevelOnly",
                    "showFilteredTags":true,
                    "showColumnTags":false,
                    "columns":[{"tag":"undated","data":{"collapsed":false,"name":"Undated"}},{"tag":"dated","data":{"collapsed":false,"name":"Do Today","range":{"tag":"before","data":1}}},{"tag":"dated","data":{"collapsed":false,"name":"Tomorrow","range":{"tag":"between","data":{"from":1,"to":1}}}},{"tag":"dated","data":{"collapsed":false,"name":"The Future","range":{"tag":"after","data":1}}},{"tag":"completed","data":{"collapsed":false,"name":"Completed","index":4,"limit":4}}],
                    "name":"date board name"},
                   {"columns":[{"tag":"untagged","data":{"collapsed":false,"name":"Untagged"}},{"tag":"namedTag","data":{"collapsed":true,"name":"name 1","tag":"tag1"}},{"tag":"completed","data":{"collapsed":false,"name":"Completed","index":2,"limit":5}}],
                    "showColumnTags":false,
                    "filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],
                    "filterPolarity":"Allow",
                    "filterScope":"SubTasksOnly",
                    "showFilteredTags":false,
                    "name":"tag board name"}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Do Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "The Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.TopLevelOnly
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board name"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "name 1" "tag1" |> Column.setCollapse True
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.SubTasksOnly
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board name"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Just "Do Today"
                                        , tomorrow = Nothing
                                        , future = Just "The Future"
                                        , undated = Nothing
                                        , otherTags = Just "The Others"
                                        , untagged = Nothing
                                        , completed = Just "Completed"
                                        }
                                    , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.fileFilter "aFile" ]
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = True
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = True
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.11.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.11.0","data":{"globalSettings":{"taskCompletionFormat":"ObsidianTasks","defaultColumnNames":{"today":"Do Today","tomorrow":"","future":"The Future","undated":"","otherTags":"The Others","untagged":"","completed":"Completed"},"ignoreFileNameDates":true},
                "boardConfigs":[
                   {"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],
                    "filterPolarity":"Deny",
                    "filterScope":"TopLevelOnly",
                    "showFilteredTags":true,
                    "showColumnTags":false,
                    "columns":[{"tag":"undated","data":{"collapsed":false,"name":"Undated"}},{"tag":"dated","data":{"collapsed":false,"name":"Do Today","range":{"tag":"before","data":1}}},{"tag":"dated","data":{"collapsed":false,"name":"Tomorrow","range":{"tag":"between","data":{"from":1,"to":1}}}},{"tag":"dated","data":{"collapsed":false,"name":"The Future","range":{"tag":"after","data":1}}},{"tag":"completed","data":{"collapsed":false,"name":"Completed","index":4,"limit":4}}],
                    "name":"date board name"},
                   {"columns":[{"tag":"untagged","data":{"collapsed":false,"name":"Untagged"}},{"tag":"namedTag","data":{"collapsed":true,"name":"name 1","tag":"tag1"}},{"tag":"completed","data":{"collapsed":false,"name":"Completed","index":2,"limit":5}}],
                    "showColumnTags":false,
                    "filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],
                    "filterPolarity":"Allow",
                    "filterScope":"SubTasksOnly",
                    "showFilteredTags":false,
                    "name":"tag board name"}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Do Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "The Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.TopLevelOnly
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board name"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "name 1" "tag1" |> Column.setCollapse True
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.SubTasksOnly
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board name"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Just "Do Today"
                                        , tomorrow = Nothing
                                        , future = Just "The Future"
                                        , undated = Nothing
                                        , otherTags = Just "The Others"
                                        , untagged = Nothing
                                        , completed = Just "Completed"
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = True
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.10.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.10.0","data":{"globalSettings":{"taskCompletionFormat":"ObsidianTasks","columnNames":{"today":"Do Today","tomorrow":"","future":"The Future","undated":"","others":"The Others","untagged":"","completed":"Completed"},"ignoreFileNameDates":true},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","filterScope":"TopLevelOnly","showFilteredTags":true,"includeUndated":true,"collapsedColumns":[],"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","filterScope":"SubTasksOnly","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"collapsedColumns":[1],"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Do Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "The Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.TopLevelOnly
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1" |> Column.setCollapse True
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.SubTasksOnly
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Just "Do Today"
                                        , tomorrow = Nothing
                                        , future = Just "The Future"
                                        , undated = Nothing
                                        , otherTags = Just "The Others"
                                        , untagged = Nothing
                                        , completed = Just "Completed"
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = True
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.9.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.9.0","data":{"globalSettings":{"taskCompletionFormat":"ObsidianTasks","columnNames":{"today":"Do Today","tomorrow":"","future":"The Future","undated":"","others":"The Others","untagged":"","completed":"Completed"}},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","filterScope":"TopLevelOnly","showFilteredTags":true,"includeUndated":true,"collapsedColumns":[],"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","filterScope":"SubTasksOnly","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"collapsedColumns":[1],"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Do Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "The Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.TopLevelOnly
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1" |> Column.setCollapse True
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.SubTasksOnly
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Just "Do Today"
                                        , tomorrow = Nothing
                                        , future = Just "The Future"
                                        , undated = Nothing
                                        , otherTags = Just "The Others"
                                        , untagged = Nothing
                                        , completed = Just "Completed"
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.8.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.8.0","data":{"globalSettings":{"taskCompletionFormat":"ObsidianTasks","columnNames":{"today":"Do Today","tomorrow":"","future":"The Future","undated":"","others":"The Others","untagged":"","completed":"Completed"}},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","filterScope":"TopLevelOnly","showFilteredTags":true,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","filterScope":"SubTasksOnly","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Do Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "The Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.TopLevelOnly
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.SubTasksOnly
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Just "Do Today"
                                        , tomorrow = Nothing
                                        , future = Just "The Future"
                                        , undated = Nothing
                                        , otherTags = Just "The Others"
                                        , untagged = Nothing
                                        , completed = Just "Completed"
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.7.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.7.0","data":{"globalSettings":{"taskCompletionFormat":"ObsidianTasks","columnNames":{"today":"Do Today","tomorrow":"","future":"The Future","undated":"","others":"The Others","untagged":"","completed":"Completed"}},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","showFilteredTags":true,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Do Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "The Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Just "Do Today"
                                        , tomorrow = Nothing
                                        , future = Just "The Future"
                                        , undated = Nothing
                                        , otherTags = Just "The Others"
                                        , untagged = Nothing
                                        , completed = Just "Completed"
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.6.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.6.0","data":{"globalSettings":{"taskCompletionFormat":"ObsidianTasks"},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","showFilteredTags":true,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Nothing
                                        , tomorrow = Nothing
                                        , future = Nothing
                                        , undated = Nothing
                                        , otherTags = Nothing
                                        , untagged = Nothing
                                        , completed = Nothing
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.5.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.5.0","data":{"globalSettings":{"taskUpdateFormat":"ObsidianTasks"},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","showFilteredTags":true,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Nothing
                                        , tomorrow = Nothing
                                        , future = Nothing
                                        , undated = Nothing
                                        , otherTags = Nothing
                                        , untagged = Nothing
                                        , completed = Nothing
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianTasks
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.4.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.4.0","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","showFilteredTags":true,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"showColumnTags":false,"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","showFilteredTags":false,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = False
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Nothing
                                        , tomorrow = Nothing
                                        , future = Nothing
                                        , undated = Nothing
                                        , otherTags = Nothing
                                        , untagged = Nothing
                                        , completed = Nothing
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.3.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.3.0","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"filterPolarity":"Deny","includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"filterPolarity":"Allow","includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Deny
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = True
                                            , showFilteredTags = True
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Nothing
                                        , tomorrow = Nothing
                                        , future = Nothing
                                        , undated = Nothing
                                        , otherTags = Nothing
                                        , untagged = Nothing
                                        , completed = Nothing
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.2.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.2.0","data":{"globalSettings":{"hideCompletedSubtasks":true,"ignorePaths":[{"tag":"pathFilter","data":"aPathToIgnore"}],"subTaskDisplayLimit":7},"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"filters":[{"tag":"pathFilter","data":"a/path"},{"tag":"tagFilter","data":"tag1"}],"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"filters":[{"tag":"pathFilter","data":"b/path"},{"tag":"tagFilter","data":"tag2"}],"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "a/path", FilterHelpers.tagFilter "tag1" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = [ FilterHelpers.pathFilter "b/path", FilterHelpers.tagFilter "tag2" ]
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = True
                                            , showFilteredTags = True
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Nothing
                                        , tomorrow = Nothing
                                        , future = Nothing
                                        , undated = Nothing
                                        , otherTags = Nothing
                                        , untagged = Nothing
                                        , completed = Nothing
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "decodes valid flags for settings version 0.1.0" <|
            \() ->
                """{"firstDayOfWeek":0,"now":11,"zone":22,"uniqueId":"12345","rightToLeft":false,"dataviewTaskCompletion":{"taskCompletionTracking":true,"taskCompletionUseEmojiShorthand":false,"taskCompletionText":"completion"},"settings":{"version":"0.1.0","data":{"boardConfigs":[{"tag":"dateBoardConfig","data":{"completedCount":4,"includeUndated":true,"title":"date board title"}},{"tag":"tagBoardConfig","data":{"columns":[{"tag":"tag 1","displayTitle":"title 1"}],"completedCount":5,"includeOthers":false,"includeUntagged":true,"title":"tag board title"}}]}}}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Expect.equal
                        (Ok
                            { settings =
                                { version = Semver.version 0 13 0 [] []
                                , boardConfigs =
                                    SafeZipper.fromList
                                        [ BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.undated "Undated"
                                                    , Column.dated (DatedColumn.init "Today" <| DatedColumn.Before 1)
                                                    , Column.dated (DatedColumn.init "Tomorrow" <| DatedColumn.Between { from = 1, to = 1 })
                                                    , Column.dated (DatedColumn.init "Future" <| DatedColumn.After 1)
                                                    , Column.completed (CompletedColumn.init "Completed" 4 4)
                                                    ]
                                            , filters = []
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = False
                                            , showFilteredTags = True
                                            , name = "date board title"
                                            }
                                        , BoardConfig
                                            { columns =
                                                Columns.fromList
                                                    [ Column.untagged "Untagged"
                                                    , Column.namedTag "title 1" "tag 1"
                                                    , Column.completed (CompletedColumn.init "Completed" 2 5)
                                                    ]
                                            , filters = []
                                            , filterPolarity = Filter.Allow
                                            , filterScope = Filter.Both
                                            , showColumnTags = True
                                            , showFilteredTags = True
                                            , name = "tag board title"
                                            }
                                        ]
                                , globalSettings =
                                    { defaultColumnNames =
                                        { today = Nothing
                                        , tomorrow = Nothing
                                        , future = Nothing
                                        , undated = Nothing
                                        , otherTags = Nothing
                                        , untagged = Nothing
                                        , completed = Nothing
                                        }
                                    , filters = []
                                    , firstDayOfWeek = GlobalSettings.FromLocale
                                    , ignoreFileNameDates = False
                                    , taskCompletionFormat = GlobalSettings.ObsidianCardBoard
                                    , taskCompletionInLocalTime = False
                                    , taskCompletionShowUtcOffset = False
                                    }
                                }
                            , dataviewTaskCompletion = DataviewTaskCompletion.Text "completion"
                            , firstDayOfWeek = 0
                            , rightToLeft = False
                            , now = 11
                            , zone = 22
                            , uniqueId = "12345"
                            }
                        )
        , test "fails to decode flags if a field is missing" <|
            \() ->
                """{"format":"a format","now":11,"zone":22}"""
                    |> DecodeHelpers.runDecoder interop.flags
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


fromElmTests : Test
fromElmTests =
    describe "interop.fromElm (encoding)"
        [ test "encodes AddFilePreviewHovers data" <|
            \() ->
                [ { filePath = "a path", id = "an id" } ]
                    |> InteropDefinitions.AddFilePreviewHovers
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"addFilePreviewHovers","data":[{"filePath":"a path","id":"an id"}]}"""
        , test "encodes CloseView" <|
            \() ->
                InteropDefinitions.CloseView
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"closeView"}"""
        , test "encodes DeleteTask data" <|
            \() ->
                { defaultTaskItemFields | filePath = "a path", lineNumber = 33, originalLine = "the text" }
                    |> InteropDefinitions.DeleteTask
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"deleteTask","data":{"filePath":"a path","lineNumber":33,"originalLine":"the text"}}"""
        , test "encodes DisplayTaskMarkdown data" <|
            \() ->
                [ { filePath = "a path", taskMarkdown = [ { id = "an id", markdown = "some markdown" } ] } ]
                    |> InteropDefinitions.DisplayTaskMarkdown
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"displayTaskMarkdown","data":[{"filePath":"a path","taskMarkdown":[{"id":"an id","markdown":"some markdown"}]}]}"""
        , test "encodes ElmInitialized" <|
            \() ->
                "a_unique_id"
                    |> InteropDefinitions.ElmInitialized
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"elmInitialized","data":"a_unique_id"}"""
        , test "encodes OpenTaskSourceFile data" <|
            \() ->
                { defaultTaskItemFields | filePath = "a path", lineNumber = 33, originalLine = "the text" }
                    |> InteropDefinitions.OpenTaskSourceFile
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"openTaskSourceFile","data":{"filePath":"a path","lineNumber":33,"originalLine":"the text"}}"""
        , test "encodes RequestFilterCandidates" <|
            \() ->
                InteropDefinitions.RequestFilterCandidates
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"requestFilterCandidates"}"""
        , test "encodes showCardContextMenu data" <|
            \() ->
                { clientPos = ( 0.1, 2.2 ), cardId = "this card" }
                    |> InteropDefinitions.ShowCardContextMenu
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"showCardContextMenu","data":{"clientPos":[0.1,2.2],"cardId":"this card"}}"""
        , test "encodes TrackDraggable data" <|
            \() ->
                { dragType = "someDragType", clientPos = { x = 1.1, y = 2.2 }, draggableId = "id of draggable" }
                    |> InteropDefinitions.TrackDraggable
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"trackDraggable","data":{"dragType":"someDragType","clientPos":{"x":1.1,"y":2.2},"draggableId":"id of draggable"}}"""
        , test "encodes UpdateTasks data" <|
            \() ->
                { filePath = "a path", tasks = [ { lineNumber = 12, originalLine = "what was there", newText = "new text" } ] }
                    |> InteropDefinitions.UpdateTasks
                    |> TsEncode.runExample interop.fromElm
                    |> .output
                    |> Expect.equal """{"tag":"updateTasks","data":{"filePath":"a path","tasks":[{"lineNumber":12,"originalLine":"what was there","newText":"new text"}]}}"""
        ]


toElmTests : Test
toElmTests =
    describe "interop.toElm (decoding)"
        [ test "decodes activeStateUpdated data" <|
            \() ->
                """{"tag":"activeStateUpdated","data":false}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.ActiveStateUpdated False)
        , test "decodes configChanged data" <|
            \() ->
                """{"tag":"configChanged","data":{"rightToLeft":true}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.ConfigChanged TextDirection.RightToLeft)
        , test "decodes elementDragged data" <|
            \() ->
                """{"tag":"elementDragged","data":{"dragType":"someDragType","dragAction":"stop","cursor":{"x":1.23,"y":4.56},"offset":{"x":1.11,"y":2.22},"draggedNodeRect":{"x":1.1,"y":2.2,"width":3.33,"height":4.44},"beacons":[{"beaconPosition":{"uniqueId":"someId","position":"before"},"rect":{"x":1.1,"y":2.2,"width":3.3,"height":4.4}}]}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal
                        (Ok <|
                            InteropDefinitions.ElementDragged
                                { dragType = "someDragType"
                                , dragAction = DragData.Stop
                                , cursor = { x = 1.23, y = 4.56 }
                                , offset = { x = 1.11, y = 2.22 }
                                , draggedNodeRect = { x = 1.1, y = 2.2, width = 3.33, height = 4.44 }
                                , beacons =
                                    [ { beaconPosition = BeaconPosition.Before "someId"
                                      , rect = { x = 1.1, y = 2.2, width = 3.3, height = 4.4 }
                                      }
                                    ]
                                }
                        )
        , test "decodes editCardDueDate data" <|
            \() ->
                """{"tag":"editCardDueDate","data":"a card"}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.EditCardDueDate "a card")
        , test "decodes filterCandidates data" <|
            \() ->
                """{"tag":"filterCandidates","data":[{"tag":"pathFilter","data":"a path"},{"tag":"pathFilter","data":"another path"}]}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.FilterCandidates [ FilterHelpers.pathFilter "a path", FilterHelpers.pathFilter "another path" ])
        , test "decodes showBoard data" <|
            \() ->
                """{"tag":"showBoard","data":17}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.ShowBoard 17)
        , test "decodes version 0.8.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.8.0","data":{"boardConfigs":[],"globalSettings":{"taskCompletionFormat":"ObsidianDataview","columnNames":{"today":"","tomorrow":"","future":"","undated":"","others":"","untagged":"","completed":""}}}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianDataview, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "decodes version 0.7.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.7.0","data":{"boardConfigs":[],"globalSettings":{"taskCompletionFormat":"ObsidianDataview","columnNames":{"today":"","tomorrow":"","future":"","undated":"","others":"","untagged":"","completed":""}}}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianDataview, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "decodes version 0.6.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.6.0","data":{"boardConfigs":[],"globalSettings":{"taskCompletionFormat":"ObsidianDataview"}}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianDataview, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "decodes version 0.5.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.5.0","data":{"boardConfigs":[],"globalSettings":{"taskUpdateFormat":"ObsidianCardBoard"}}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianCardBoard, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "decodes version 0.4.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.4.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianCardBoard, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "decodes version 0.3.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.3.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianCardBoard, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "decodes version 0.2.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.2.0","data":{"boardConfigs":[],"globalSettings":{"hideCompletedSubtasks":false,"ignorePaths":[],"subTaskDisplayLimit":null}}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianCardBoard, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "decodes version 0.1.0 settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"0.1.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.SettingsUpdated { version = Semver.version 0 13 0 [] [], boardConfigs = SafeZipper.fromList [], globalSettings = { defaultColumnNames = DefaultColumnNames.default, filters = [], firstDayOfWeek = GlobalSettings.FromLocale, ignoreFileNameDates = False, taskCompletionFormat = GlobalSettings.ObsidianCardBoard, taskCompletionInLocalTime = False, taskCompletionShowUtcOffset = False } })
        , test "fails to decode an unsupported version of settings data" <|
            \() ->
                """{"tag":"settingsUpdated","data":{"version":"99999.0.0","data":{"boardConfigs":[]}}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "fails to decode data with an unknown tag" <|
            \() ->
                """{"tag":"xxxxx","data":{"filePath":"a path","fileDate":"a date","fileContents":"some contents"}}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "decodes taskItemsAdded data" <|
            \() ->
                let
                    taskList : TaskList
                    taskList =
                        TaskList.empty
                            |> TaskList.add (taskItem "- [ ] foo")
                in
                """{"tag":"taskItemsAdded","data":[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] foo","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]}]}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.TaskItemsAdded taskList)
        , test "decodes taskItemsDeleted data" <|
            \() ->
                """{"tag":"taskItemsDeleted","data":["foo","bar","baz"]}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.TaskItemsDeleted [ "foo", "bar", "baz" ])
        , test "decodes taskItemsDeletedAndAdded data" <|
            \() ->
                let
                    toAdd : List TaskItem
                    toAdd =
                        TaskList.empty
                            |> TaskList.add (taskItem "- [ ] bar")
                            |> TaskList.toList
                in
                """{"tag":"taskItemsDeletedAndAdded","data":[["taskId:1"],[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"bar"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] bar","originalLine":"- [ ] bar","tags":[],"title":["bar"]},"subFields":[]}]]}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.TaskItemsDeletedAndAdded ( [ "taskId:1" ], toAdd ))
        , test "decodes taskItemsRefreshed data" <|
            \() ->
                let
                    taskList : TaskList
                    taskList =
                        TaskList.empty
                            |> TaskList.add (taskItem "- [ ] foo")
                in
                """{"tag":"taskItemsRefreshed","data":[{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] foo","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]}]}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.TaskItemsRefreshed taskList)
        , test "decodes taskItemsUpdated data" <|
            \() ->
                """{"tag":"taskItemsUpdated","data":[["bar",{"fields":{"autoComplete":{"tag":"NotSpecifed"},"completion":{"tag":"Incomplete"},"contents":[{"tag":"Word","data":"foo"}],"dueFile":null,"dueTag":{"tag":"NotSet"},"filePath":"","lineNumber":1,"notes":"","originalBlock":"- [ ] foo","originalLine":"- [ ] foo","tags":[],"title":["foo"]},"subFields":[]}]]}"""
                    |> DecodeHelpers.runDecoder interop.toElm
                    |> .decoded
                    |> Expect.equal (Ok <| InteropDefinitions.TaskItemsUpdated [ ( "bar", taskItem "- [ ] foo" ) ])
        ]



-- HELPERS


defaultTaskItemFields : TaskItemFields
defaultTaskItemFields =
    TaskItem.fields TaskItem.dummy


taskItem : String -> TaskItem
taskItem markdown =
    Parser.run TaskItemHelpers.basicParser markdown
        |> Result.withDefault TaskItem.dummy
