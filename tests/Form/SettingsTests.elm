module Form.SettingsTests exposing (suite)

import BoardConfig
import Column
import Column.Completed as CompletedColumn
import Column.Undated as UndatedColumn
import Column.Untagged as UntaggedColumn
import Columns
import DefaultColumnNames
import Expect
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Columns as ColumnsForm
import Form.SafeDecoder as SD
import Form.Settings as SettingsForm exposing (SettingsForm)
import GlobalSettings
import SafeZipper
import Settings
import Test exposing (..)


suite : Test
suite =
    concat
        [ safeDecoder
        ]


safeDecoder : Test
safeDecoder =
    describe "safeDecoder"
        [ test "decodes an empty boardConfigForms" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.empty }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map .boardConfigs
                    |> Result.map SafeZipper.toList
                    |> Result.withDefault []
                    |> List.map BoardConfig.name
                    |> Expect.equal []
        , test "decodes a boardConfigForms with configs for multiple boards" <|
            \() ->
                { exampleSettingsForm
                    | boardConfigForms =
                        SafeZipper.fromList
                            [ exampleBoardConfigForm1
                            , exampleBoardConfigForm2
                            , exampleBoardConfigForm3
                            ]
                }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map .boardConfigs
                    |> Result.map SafeZipper.toList
                    |> Result.withDefault []
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "board 1", "board 2", "board 3" ]
        , test "decodes a completed string as Just the string" <|
            \() ->
                { exampleSettingsForm | completed = "foo" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "completed")
                    |> Expect.equal (Ok "foo")
        , test "trims whitespace from a completed string" <|
            \() ->
                { exampleSettingsForm | completed = "  foo  " }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "completed")
                    |> Expect.equal (Ok "foo")
        , test "decodes an empty completed string as Nothing" <|
            \() ->
                { exampleSettingsForm | completed = "" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "completed")
                    |> Expect.equal (Ok "Completed")
        , test "decodes a future string as Just the string" <|
            \() ->
                { exampleSettingsForm | future = "foo" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "future")
                    |> Expect.equal (Ok "foo")
        , test "trims whitespace from a future string" <|
            \() ->
                { exampleSettingsForm | future = "  foo  " }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "future")
                    |> Expect.equal (Ok "foo")
        , test "decodes an empty future string as Nothing" <|
            \() ->
                { exampleSettingsForm | future = "" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "future")
                    |> Expect.equal (Ok "Future")
        , test "decodes a False ignoreFileNameDates" <|
            \() ->
                { exampleSettingsForm | ignoreFileNameDates = False }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .ignoreFileNameDates
                    |> Expect.equal (Ok False)
        , test "decodes a True ignoreFileNameDates" <|
            \() ->
                { exampleSettingsForm | ignoreFileNameDates = True }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .ignoreFileNameDates
                    |> Expect.equal (Ok True)
        , test "decodes a otherTags string as Just the string" <|
            \() ->
                { exampleSettingsForm | otherTags = "foo" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "otherTags")
                    |> Expect.equal (Ok "foo")
        , test "trims whitespace from a otherTags string" <|
            \() ->
                { exampleSettingsForm | otherTags = "  foo  " }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "otherTags")
                    |> Expect.equal (Ok "foo")
        , test "decodes an empty otherTags string as Nothing" <|
            \() ->
                { exampleSettingsForm | otherTags = "" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "otherTags")
                    |> Expect.equal (Ok "Other Tags")
        , test "decodes the NoCompletion TaskCompletionFormat" <|
            \() ->
                { exampleSettingsForm | taskCompletionFormat = "NoCompletion" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .taskCompletionFormat
                    |> Expect.equal (Ok <| GlobalSettings.NoCompletion)
        , test "decodes the ObsidianCardBoard TaskCompletionFormat" <|
            \() ->
                { exampleSettingsForm | taskCompletionFormat = "ObsidianCardBoard" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .taskCompletionFormat
                    |> Expect.equal (Ok <| GlobalSettings.ObsidianCardBoard)
        , test "decodes the ObsidianDataview TaskCompletionFormat" <|
            \() ->
                { exampleSettingsForm | taskCompletionFormat = "ObsidianDataview" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .taskCompletionFormat
                    |> Expect.equal (Ok <| GlobalSettings.ObsidianDataview)
        , test "decodes the ObsidianTasks TaskCompletionFormat" <|
            \() ->
                { exampleSettingsForm | taskCompletionFormat = "ObsidianTasks" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .taskCompletionFormat
                    |> Expect.equal (Ok <| GlobalSettings.ObsidianTasks)
        , test "decodes an invalid TaskCompletionFormat as ObsidianCardBoard" <|
            \() ->
                { exampleSettingsForm | taskCompletionFormat = "xxxxxxxxxxxxx" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .taskCompletionFormat
                    |> Expect.equal (Ok <| GlobalSettings.ObsidianCardBoard)
        , test "decodes a today string as Just the string" <|
            \() ->
                { exampleSettingsForm | today = "foo" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "today")
                    |> Expect.equal (Ok "foo")
        , test "trims whitespace from a today string" <|
            \() ->
                { exampleSettingsForm | today = "  foo  " }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "today")
                    |> Expect.equal (Ok "foo")
        , test "decodes an empty today string as Nothing" <|
            \() ->
                { exampleSettingsForm | today = "" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "today")
                    |> Expect.equal (Ok "Today")
        , test "decodes a tomorrow string as Just the string" <|
            \() ->
                { exampleSettingsForm | tomorrow = "foo" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "tomorrow")
                    |> Expect.equal (Ok "foo")
        , test "trims whitespace from a tomorrow string" <|
            \() ->
                { exampleSettingsForm | tomorrow = "  foo  " }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "tomorrow")
                    |> Expect.equal (Ok "foo")
        , test "decodes an empty tomorrow string as Nothing" <|
            \() ->
                { exampleSettingsForm | tomorrow = "" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "tomorrow")
                    |> Expect.equal (Ok "Tomorrow")
        , test "decodes a undated string as Just the string" <|
            \() ->
                { exampleSettingsForm | undated = "foo" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "undated")
                    |> Expect.equal (Ok "foo")
        , test "trims whitespace from a undated string" <|
            \() ->
                { exampleSettingsForm | undated = "  foo  " }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "undated")
                    |> Expect.equal (Ok "foo")
        , test "decodes an empty undated string as Nothing" <|
            \() ->
                { exampleSettingsForm | undated = "" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "undated")
                    |> Expect.equal (Ok "Undated")
        , test "decodes a untagged string as Just the string" <|
            \() ->
                { exampleSettingsForm | untagged = "foo" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "untagged")
                    |> Expect.equal (Ok "foo")
        , test "trims whitespace from a untagged string" <|
            \() ->
                { exampleSettingsForm | untagged = "  foo  " }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "untagged")
                    |> Expect.equal (Ok "foo")
        , test "decodes an empty untagged string as Nothing" <|
            \() ->
                { exampleSettingsForm | untagged = "" }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map Settings.globalSettings
                    |> Result.map .defaultColumnNames
                    |> Result.map (DefaultColumnNames.nameFor "untagged")
                    |> Expect.equal (Ok "Untagged")
        ]



-- HELPERS


exampleSettingsForm : SettingsForm
exampleSettingsForm =
    { boardConfigForms = SafeZipper.empty
    , completed = ""
    , future = ""
    , ignoreFileNameDates = False
    , otherTags = ""
    , taskCompletionFormat = ""
    , today = ""
    , tomorrow = ""
    , undated = ""
    , untagged = ""
    }


exampleBoardConfigForm1 : BoardConfigForm
exampleBoardConfigForm1 =
    { columns = ColumnsForm.init <| Columns.fromList [ Column.completed <| CompletedColumn.init "" 0 10 ]
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = "board 1"
    , showColumnTags = False
    , showFilteredTags = False
    }


exampleBoardConfigForm2 : BoardConfigForm
exampleBoardConfigForm2 =
    { columns = ColumnsForm.init <| Columns.fromList [ Column.otherTags "" [] ]
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = "board 2"
    , showColumnTags = False
    , showFilteredTags = False
    }


exampleBoardConfigForm3 : BoardConfigForm
exampleBoardConfigForm3 =
    { columns = ColumnsForm.init <| Columns.fromList [ Column.undated "", Column.untagged "" ]
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = "board 3"
    , showColumnTags = False
    , showFilteredTags = False
    }
