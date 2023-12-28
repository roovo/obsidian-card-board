module Form.SettingsTests exposing (suite)

import BoardConfig
import Column
import Column.Completed as CompletedColumn
import Column.Undated as UndatedColumn
import Column.Untagged as UntaggedColumn
import Columns
import DefaultColumnNames
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import Expect
import Form.BoardConfig as BoardConfigForm exposing (BoardConfigForm)
import Form.Column as ColumnForm
import Form.Columns as ColumnsForm
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Form.SafeDecoder as SD
import Form.Settings as SettingsForm exposing (SettingsForm)
import GlobalSettings
import SafeZipper
import Settings
import Test exposing (..)


suite : Test
suite =
    concat
        [ addBoard
        , defaultColumnNames
        , deleteColumn
        , mapCurrentBoard
        , mapCurrentColumnsForm
        , moveBoard
        , safeDecoder
        , toggleIgnoreFileNameDate
        , updateDefaultColumnName
        , updateTaskCompletionFormat
        ]


addBoard : Test
addBoard =
    describe "addBoard"
        [ test "adds an emptyBoard to a settingsForm with no boards" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.empty }
                    |> SettingsForm.addBoard DefaultColumnNames.default (NewBoardForm "foo" "emptyBoard")
                    |> SettingsForm.boardConfigForms
                    |> SafeZipper.toList
                    |> List.map .name
                    |> Expect.equal [ "foo" ]
        , test "adds new boards to the end of the list" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.fromList [ exampleBoardConfigForm3 ] }
                    |> SettingsForm.addBoard DefaultColumnNames.default (NewBoardForm "foo" "emptyBoard")
                    |> SettingsForm.boardConfigForms
                    |> SafeZipper.toList
                    |> List.map .name
                    |> Expect.equal [ "board 3", "foo" ]
        , test "is focussed on the new board" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.fromList [ exampleBoardConfigForm3 ] }
                    |> SettingsForm.addBoard DefaultColumnNames.default (NewBoardForm "foo" "emptyBoard")
                    |> SettingsForm.boardConfigForms
                    |> SafeZipper.current
                    |> Maybe.map .name
                    |> Expect.equal (Just "foo")
        ]


defaultColumnNames : Test
defaultColumnNames =
    describe "defaultColumnNames"
        [ test "uses the today value" <|
            \() ->
                { exampleSettingsForm | today = "foo" }
                    |> SettingsForm.defaultColumnNames
                    |> DefaultColumnNames.nameFor "today"
                    |> Expect.equal "foo"
        , test "uses the tomorrow value" <|
            \() ->
                { exampleSettingsForm | tomorrow = "foo" }
                    |> SettingsForm.defaultColumnNames
                    |> DefaultColumnNames.nameFor "tomorrow"
                    |> Expect.equal "foo"
        , test "uses the future value" <|
            \() ->
                { exampleSettingsForm | future = "foo" }
                    |> SettingsForm.defaultColumnNames
                    |> DefaultColumnNames.nameFor "future"
                    |> Expect.equal "foo"
        , test "uses the undated value" <|
            \() ->
                { exampleSettingsForm | undated = "foo" }
                    |> SettingsForm.defaultColumnNames
                    |> DefaultColumnNames.nameFor "undated"
                    |> Expect.equal "foo"
        , test "uses the otherTags value" <|
            \() ->
                { exampleSettingsForm | otherTags = "foo" }
                    |> SettingsForm.defaultColumnNames
                    |> DefaultColumnNames.nameFor "otherTags"
                    |> Expect.equal "foo"
        , test "uses the untagged value" <|
            \() ->
                { exampleSettingsForm | untagged = "foo" }
                    |> SettingsForm.defaultColumnNames
                    |> DefaultColumnNames.nameFor "untagged"
                    |> Expect.equal "foo"
        , test "uses the completed value" <|
            \() ->
                { exampleSettingsForm | completed = "foo" }
                    |> SettingsForm.defaultColumnNames
                    |> DefaultColumnNames.nameFor "completed"
                    |> Expect.equal "foo"
        ]


deleteColumn : Test
deleteColumn =
    describe "deleteColumn"
        [ test "does nothing if there are no boards" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.empty }
                    |> SettingsForm.deleteColumn 0
                    |> .boardConfigForms
                    |> SafeZipper.toList
                    |> Expect.equal []
        , test "does nothing if there are is a board but there is no column at the given index" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.fromList [ exampleBoardConfigForm3 ] }
                    |> SettingsForm.deleteColumn 10
                    |> .boardConfigForms
                    |> SafeZipper.current
                    |> Maybe.map .columns
                    |> Maybe.map .columnForms
                    |> Maybe.withDefault []
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo", "bar" ]
        , test "deletes a column at the given index" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.fromList [ exampleBoardConfigForm3 ] }
                    |> SettingsForm.deleteColumn 1
                    |> .boardConfigForms
                    |> SafeZipper.current
                    |> Maybe.map .columns
                    |> Maybe.map .columnForms
                    |> Maybe.withDefault []
                    |> List.map ColumnForm.name
                    |> Expect.equal [ "foo" ]
        ]


mapCurrentBoard : Test
mapCurrentBoard =
    describe "mapCurrentBoard"
        [ test "does nothing if there are no boards" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.empty }
                    |> SettingsForm.mapCurrentBoard (\bc -> { bc | name = "new name" })
                    |> .boardConfigForms
                    |> SafeZipper.toList
                    |> Expect.equal []
        , test "applies the map function to the current board if there is one" <|
            \() ->
                { exampleSettingsForm
                    | boardConfigForms =
                        SafeZipper.fromList
                            [ exampleBoardConfigForm1
                            , exampleBoardConfigForm2
                            , exampleBoardConfigForm3
                            ]
                            |> SafeZipper.atIndex 1
                }
                    |> SettingsForm.mapCurrentBoard (\bc -> { bc | name = "new name" })
                    |> .boardConfigForms
                    |> SafeZipper.current
                    |> Maybe.map .name
                    |> Expect.equal (Just "new name")
        ]


mapCurrentColumnsForm : Test
mapCurrentColumnsForm =
    describe "mapCurrentColumnsForm"
        [ test "does nothing if there are no boards" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.empty }
                    |> SettingsForm.mapCurrentColumnsForm (always ColumnsForm.empty)
                    |> .boardConfigForms
                    |> SafeZipper.toList
                    |> Expect.equal []

        -- , test "applies the map function to the current board if there is one" <|
        --     \() ->
        --         { exampleSettingsForm
        --             | boardConfigForms =
        --                 SafeZipper.fromList
        --                     [ exampleBoardConfigForm1
        --                     , exampleBoardConfigForm2
        --                     , exampleBoardConfigForm3
        --                     ]
        --                     |> SafeZipper.atIndex 1
        --         }
        --             |> SettingsForm.mapCurrentColumnsForm (\bc -> { bc | name = "new name" })
        --             |> .boardConfigForms
        --             |> SafeZipper.current
        --             |> Maybe.map .name
        --             |> Expect.equal (Just "new name")
        ]


moveBoard : Test
moveBoard =
    describe "moveBoard"
        [ test "does nothing if there are no boards" <|
            \() ->
                { exampleSettingsForm | boardConfigForms = SafeZipper.empty }
                    |> SettingsForm.moveBoard "0" (BeaconPosition.After "0")
                    |> SettingsForm.boardConfigForms
                    |> Expect.equal SafeZipper.empty
        , test "moves a board" <|
            \() ->
                { exampleSettingsForm
                    | boardConfigForms =
                        SafeZipper.fromList
                            [ exampleBoardConfigForm1, exampleBoardConfigForm2, exampleBoardConfigForm3 ]
                }
                    |> SettingsForm.moveBoard "board 2" (BeaconPosition.Before "board 1")
                    |> SettingsForm.boardConfigForms
                    |> SafeZipper.toList
                    |> List.map .name
                    |> Expect.equal [ "board 2", "board 1", "board 3" ]
        , test "sets the moved board as the current board after moving" <|
            \() ->
                { exampleSettingsForm
                    | boardConfigForms =
                        SafeZipper.fromList
                            [ exampleBoardConfigForm1, exampleBoardConfigForm2, exampleBoardConfigForm3 ]
                }
                    |> SettingsForm.moveBoard "board 2" (BeaconPosition.Before "board 1")
                    |> SettingsForm.boardConfigForms
                    |> SafeZipper.current
                    |> Maybe.map .name
                    |> Expect.equal (Just "board 2")
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
        , test "preserves the position when there are multiple boards" <|
            \() ->
                { exampleSettingsForm
                    | boardConfigForms =
                        SafeZipper.fromList
                            [ exampleBoardConfigForm1
                            , exampleBoardConfigForm2
                            , exampleBoardConfigForm3
                            ]
                            |> SafeZipper.atIndex 1
                }
                    |> SD.run SettingsForm.safeDecoder
                    |> Result.map .boardConfigs
                    |> Result.map SafeZipper.currentIndex
                    |> Result.map (Maybe.withDefault -1)
                    |> Expect.equal (Ok 1)
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


toggleIgnoreFileNameDate : Test
toggleIgnoreFileNameDate =
    describe "toggleIgnoreFileNameDate"
        [ test "toggles the value from True to False" <|
            \() ->
                { exampleSettingsForm | ignoreFileNameDates = True }
                    |> SettingsForm.toggleIgnoreFileNameDate
                    |> .ignoreFileNameDates
                    |> Expect.equal False
        , test "toggles the value from False to False" <|
            \() ->
                { exampleSettingsForm | ignoreFileNameDates = False }
                    |> SettingsForm.toggleIgnoreFileNameDate
                    |> .ignoreFileNameDates
                    |> Expect.equal True
        ]


updateDefaultColumnName : Test
updateDefaultColumnName =
    describe "updateDefaultColumnName"
        [ test "updates the today field" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateDefaultColumnName "today" "foo"
                    |> .today
                    |> Expect.equal "foo"
        , test "updates the tomorrow field" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateDefaultColumnName "tomorrow" "foo"
                    |> .tomorrow
                    |> Expect.equal "foo"
        , test "updates the future field" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateDefaultColumnName "future" "foo"
                    |> .future
                    |> Expect.equal "foo"
        , test "updates the undated field" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateDefaultColumnName "undated" "foo"
                    |> .undated
                    |> Expect.equal "foo"
        , test "updates the otherTags field" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateDefaultColumnName "otherTags" "foo"
                    |> .otherTags
                    |> Expect.equal "foo"
        , test "updates the untagged field" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateDefaultColumnName "untagged" "foo"
                    |> .untagged
                    |> Expect.equal "foo"
        , test "updates the completed field" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateDefaultColumnName "completed" "foo"
                    |> .completed
                    |> Expect.equal "foo"
        ]


updateTaskCompletionFormat : Test
updateTaskCompletionFormat =
    describe "updateTaskCompletionFormat"
        [ test "updates the format" <|
            \() ->
                exampleSettingsForm
                    |> SettingsForm.updateTaskCompletionFormat "new format"
                    |> .taskCompletionFormat
                    |> Expect.equal "new format"
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
    { columns = ColumnsForm.init <| Columns.fromList [ Column.undated "foo", Column.untagged "bar" ]
    , filters = []
    , filterPolarity = ""
    , filterScope = ""
    , name = "board 3"
    , showColumnTags = False
    , showFilteredTags = False
    }
