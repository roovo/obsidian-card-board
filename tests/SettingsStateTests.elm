module SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import Column
import Column.Completed as CompletedColumn
import Columns
import DefaultColumnNames
import Expect
import Filter
import Form.BoardConfig exposing (BoardConfigForm)
import Form.Column as ColumnForm exposing (ColumnForm)
import Form.Columns as ColumnsForm exposing (ColumnsForm)
import Form.NewBoard as NewBoardForm exposing (NewBoardForm)
import Form.NewColumn as NewColumnForm exposing (NewColumnForm)
import Form.Settings as SettingsForm exposing (SettingsForm)
import GlobalSettings exposing (GlobalSettings)
import SafeZipper
import Settings exposing (Settings)
import SettingsState exposing (SettingsState)
import Test exposing (..)


suite : Test
suite =
    concat
        -- [ addBoardConfirmed
        -- , addBoardRequested
        -- , addColumnConfirmed
        -- , addColumnRequested
        -- , boardConfigs
        -- , cancelCurrentState
        -- , deleteConfirmed
        -- , deleteBoardRequested
        -- , deleteColumnRequested
        -- , editBoardAt
        -- , editGlobalSettings
        [ init

        -- , mapBoardBeingAdded
        , mapBoardBeingEdited

        -- , mapColumnBeingAdded
        -- , mapCurrentColumnsForm
        -- , mapGlobalSettings
        ]



-- addBoardConfirmed : Test
-- addBoardConfirmed =
--     describe "addBoardConfirmed"
--         -- [ test "AddingBoard -> EditingBoard focussed on the new board which is on the end" <|
--         --     \() ->
--         --         SettingsState.AddingBoard exampleNewBoard
--         --             (settingsFromBoardConfigs [ exampleBoardConfig ])
--         --             SettingsForm.empty
--         --             |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--         --             |> Expect.equal
--         --                 (SettingsState.EditingBoard
--         --                     (settingsFromBoardConfigsWithIndex 1
--         --                         [ exampleBoardConfig
--         --                         , BoardConfig.fromNewBoard DefaultColumnNames.default exampleNewBoard
--         --                         ]
--         --                     )
--         --                     SettingsForm.empty
--         --                 )
--         -- , test "AddingBoard -> EditingBoard changes blank name to Unnamed" <|
--         --     \() ->
--         --         SettingsState.AddingBoard noNameNewBoard
--         --             (settingsFromBoardConfigs [ exampleBoardConfig ])
--         --             SettingsForm.empty
--         --             |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--         --             |> Expect.equal
--         --                 (SettingsState.EditingBoard
--         --                     (settingsFromBoardConfigsWithIndex 1
--         --                         [ exampleBoardConfig
--         --                         , BoardConfig.fromNewBoard DefaultColumnNames.default unnamedNameNewBoard
--         --                         ]
--         --                     )
--         --                     SettingsForm.empty
--         --                 )
--         [ test "does nothing if AddingColumn" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty)
--         , test "does nothing if ClosingPlugin" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--                     |> Expect.equal (SettingsState.ClosingPlugin Settings.default SettingsForm.empty)
--         , test "does nothing if ClosingSettings" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         , test "does nothing if DeletingBoard" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if DeletingColumn" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--                     |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty)
--         , test "does nothing if EditingBoard" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardConfirmed DefaultColumnNames.default
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         ]
--
--
-- addBoardRequested : Test
-- addBoardRequested =
--     describe "addBoardRequested"
--         [ test "does nothing if already in AddingBoard state" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "AddingColumn -> AddingBoard" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "ClosingPlugin -> AddingBoard" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "ClosingSettings -> AddingBoard" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "DeletingBoard -> AddingBoard" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "DeletingColumn -> AddingBoard" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "EditingBoard -> AddingBoard" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "EditingGlobalSettings -> AddingBoard" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addBoardRequested
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         ]
--
--
-- addColumnConfirmed : Test
-- addColumnConfirmed =
--     describe "addColumnConfirmed"
--         [ test "does nothing if AddingBoard" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "AddingColumn -> EditingBoard" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if ClosingPlugin" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.ClosingPlugin Settings.default SettingsForm.empty)
--         , test "does nothing if ClosingSettings" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         , test "does nothing if DeletingBoard" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if DeletingColumn" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty)
--         , test "does nothing if EditingBoard" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnConfirmed
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         ]
--
--
-- addColumnRequested : Test
-- addColumnRequested =
--     describe "addColumnRequested"
--         [ test "AddingBoard -> AddingColumn" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         , test "the NewColumnForm defaults to the first dropdown item" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> newColumnConfigForm
--                     |> Maybe.map .columnType
--                     |> Expect.equal (Just "completed")
--         , test "does nothing if already in AddingColumn state" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         , test "ClosingPlugin -> AddingColumn" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         , test "ClosingSettings -> AddingColumn" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         , test "DeletingBoard -> AddingColumn" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         , test "DeletingColumn -> AddingColumn" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         , test "EditingBoard -> AddingColumn" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         , test "EditingGlobalSettings -> AddingColumn" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.addColumnRequested
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "completed") Settings.default SettingsForm.empty)
--         ]
--
--
-- boardConfigs : Test
-- boardConfigs =
--     describe "boardConfigs"
--         [ test "returns the configs (apart from the one being added) if in AddingBoard state" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         , test "returns the configs if in AddingColumn state" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         , test "returns the configs if in ClosingPlugin state" <|
--             \() ->
--                 SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         , test "returns the configs if in ClosingSettings state" <|
--             \() ->
--                 SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         , test "returns the configs if in DeletingBoard state" <|
--             \() ->
--                 SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         , test "returns the configs if in DeletingColumn state" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         , test "returns the configs if in EditingBoard state" <|
--             \() ->
--                 SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         , test "returns the configs if in EditingGlobalSettings state" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.boardConfigs
--                     |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
--         ]
--
--
-- cancelCurrentState : Test
-- cancelCurrentState =
--     describe "cancelCurrentState"
--         [ test "AddingBoard -> ClosingPlugin if the board list is empty" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.ClosingPlugin Settings.default SettingsForm.empty)
--         , test "AddingBoard -> EditingBoard if the board list is NOT empty" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigs
--                                 [ exampleBoardConfig ]
--                             )
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] } ]
--                             }
--                         )
--         , test "AddingColumn -> EditingBoard" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty)
--         , test "ClosingPlugin -> ClosingPlugin" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.ClosingPlugin Settings.default SettingsForm.empty)
--         , test "ClosingSettings -> ClosingSettings" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         , test "DeletingBoard -> EditingBoard" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "DeletingColumn -> EditingBoard" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "EditingBoard -> ClosingSettings" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         , test "EditingGlobalSettings -> ClosingSettings" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.cancelCurrentState
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         ]
--
--
-- deleteBoardRequested : Test
-- deleteBoardRequested =
--     describe "deleteBoardRequested"
--         [ test "AddingBoard -> DeletingBoard" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "AddingColumn -> DeletingBoard" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "ClosingPlugin -> DeletingBoard" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "ClosingSettings -> DeletingBoard" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if already DeletingBoard" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "DeletingColumn -> DeletingBoard" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "EditingBoard -> DeletingBoard" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "EditingGlobalSettings -> DeletingBoard" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.deleteBoardRequested
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         ]
--
--
-- deleteColumnRequested : Test
-- deleteColumnRequested =
--     describe "deleteColumnRequested"
--         [ test "AddingBoard -> DeletingColumn" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         , test "AddingColumn -> DeletingColumn" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         , test "ClosingPlugin -> DeletingColumn" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         , test "ClosingSettings -> DeletingColumn" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         , test "DeletingBoard -> DeletingColumn" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         , test "ensure deleting the given column if already in DeletingColumn state" <|
--             \() ->
--                 SettingsState.DeletingColumn 2 Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         , test "EditingBoard -> DeletingColumn" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         , test "EditingGlobalSettings -> DeletingColumn" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.deleteColumnRequested 3
--                     |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default SettingsForm.empty)
--         ]
--
--
-- deleteConfirmed : Test
-- deleteConfirmed =
--     describe "deleteConfirmed"
--         [ test "does nothing if AddingBoard" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "does nothing if AddingColumn" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty)
--         , test "does nothing if ClosingPlugin" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.ClosingPlugin Settings.default SettingsForm.empty)
--         , test "does nothing if ClosingSettings" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         , test "deletes the current board and -> Adding if DeletingBoard and there is ONLY one board" <|
--             \() ->
--                 SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) SettingsForm.empty
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--
--         -- , test "deletes the current board and -> EditingBoard if DeletingBoard and there is MORE THAN one board" <|
--         --     \() ->
--         --         SettingsState.DeletingBoard
--         --             (settingsFromBoardConfigsWithIndex 1
--         --                 [ exampleBoardConfigNoColumns, exampleBoardConfig, exampleBoardConfigNoColumns ]
--         --             )
--         --             SettingsForm.empty
--         --             |> SettingsState.deleteConfirmed
--         --             |> Expect.equal
--         --                 (SettingsState.EditingBoard
--         --                     (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfigNoColumns ])
--         --                     { columnsForms =
--         --                         SafeZipper.fromList
--         --                             [ { columnForms = [] }
--         --                             , { columnForms = [] }
--         --                             ]
--         --                             |> SafeZipper.atIndex 1
--         --                     }
--         --                 )
--         , test "deletes the current column and -> EditingBoard if DeletingColumn" <|
--             \() ->
--                 let
--                     result =
--                         SafeZipper.fromList
--                             [ BoardConfig.fromNewBoard DefaultColumnNames.default (NewBoardForm "foo" "emptyBoard")
--                             , BoardConfig.fromNewBoard DefaultColumnNames.default (NewBoardForm "baz" "tagBoard")
--                                 |> BoardConfig.deleteColumn 1
--                             ]
--                             |> SafeZipper.atIndex 1
--                             |> SettingsForm.init
--                 in
--                 SettingsState.DeletingColumn 1 (settingsFromBoardConfigs [ exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) result)
--         , test "does nothing if EditingBoard" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.deleteConfirmed
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         ]
--
--
-- editBoardAt : Test
-- editBoardAt =
--     describe "editBoardAt"
--         [ test "AddingBoard -> EditingBoard" <|
--             \() ->
--                 SettingsState.AddingBoard
--                     NewBoardForm.default
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         , test "AddingColumn -> EditingBoard" <|
--             \() ->
--                 SettingsState.AddingColumn
--                     (NewColumnForm "" "")
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         , test "ClosingPlugin -> EditingBoard" <|
--             \() ->
--                 SettingsState.ClosingPlugin
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         , test "ClosingSettings -> EditingBoard" <|
--             \() ->
--                 SettingsState.ClosingSettings
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         , test "DeletingBoard -> EditingBoard" <|
--             \() ->
--                 SettingsState.DeletingBoard
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         , test "DeletingColumn -> EditingBoard" <|
--             \() ->
--                 SettingsState.DeletingColumn
--                     1
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         , test "EditingBoard -> EditingBoard (switched)" <|
--             \() ->
--                 SettingsState.EditingBoard
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         , test "EditingGlobalSettings -> EditingBoard" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings
--                     (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                     { columnsForms =
--                         SafeZipper.fromList
--                             [ { columnForms = [] }
--                             , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                             ]
--                     }
--                     |> SettingsState.editBoardAt 1
--                     |> Expect.equal
--                         (SettingsState.EditingBoard
--                             (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ])
--                             { columnsForms =
--                                 SafeZipper.fromList
--                                     [ { columnForms = [] }
--                                     , { columnForms = [ ColumnForm.NamedTagColumnForm { name = "foo", tag = "bar" } ] }
--                                     ]
--                                     |> SafeZipper.atIndex 1
--                             }
--                         )
--         ]
--
--
-- editGlobalSettings : Test
-- editGlobalSettings =
--     describe "editGlobalSettings"
--         [ test "AddingBoard -> EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         , test "AddingColumn -> EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         , test "ClosingPlugin -> EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         , test "ClosingSettings -> EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         , test "DeletingBoard -> EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         , test "DeletingColumn -> EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         , test "EditingBoard -> EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         , test "does nothing to EditingGlobalSettings" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.editGlobalSettings
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard with the default NewBoardForm if there are no existing board configs" <|
            \() ->
                SettingsState.init Settings.default
                    |> newBoardFormBeingAdded
                    |> Expect.equal (Just NewBoardForm.default)
        , test "returns AddingBoard with no board configs if there are no existing board configs" <|
            \() ->
                SettingsState.init Settings.default
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> Expect.equal SafeZipper.empty
        , test "returns EditingBoard with the board configs if there are some board configs" <|
            \() ->
                SettingsState.init settingsWithBoards
                    |> SettingsState.settings
                    |> Settings.boardConfigs
                    |> SafeZipper.toList
                    |> List.map BoardConfig.name
                    |> Expect.equal [ "board 1", "board 2", "board 3" ]
        ]



-- mapBoardBeingAdded : Test
-- mapBoardBeingAdded =
--     describe "mapBoardBeingAdded"
--         [ test "maps the board being added if it is in the AddingBoard state" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.AddingBoard exampleNewBoard Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the AddingColumn state" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the ClosingPlugin state" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.ClosingPlugin Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the ClosingSettings state" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the DeletingBoard state" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the DeletingColumn state" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the EditingBoard state" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the EditingGlobalSettings state" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.mapBoardBeingAdded (always exampleNewBoard)
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         ]
--
--


exampleSettingsFormWithThreeBoards : SettingsForm
exampleSettingsFormWithThreeBoards =
    { exampleSettingsForm
        | boardConfigForms =
            SafeZipper.fromList [ exampleBoardConfigForm1, exampleBoardConfigForm2, exampleBoardConfigForm3 ]
                |> SafeZipper.atIndex 1
    }


mapBoardBeingEdited : Test
mapBoardBeingEdited =
    describe "mapBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                let
                    initialState =
                        SettingsState.AddingBoard NewBoardForm.default exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                let
                    initialState =
                        SettingsState.AddingColumn NewColumnForm.default exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                let
                    initialState =
                        SettingsState.ClosingPlugin exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                let
                    initialState =
                        SettingsState.ClosingSettings exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                let
                    initialState =
                        SettingsState.DeletingBoard exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                let
                    initialState =
                        SettingsState.DeletingColumn 0 exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "does nothing if it is in the EditingBoard state and there are no boards" <|
            \() ->
                let
                    initialState =
                        SettingsState.EditingBoard exampleSettingsForm
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        , test "updates the current board if it is in the EditingBoard state and there is a current board" <|
            \() ->
                let
                    initialState =
                        SettingsState.EditingBoard exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal
                        (SettingsState.EditingBoard
                            { exampleSettingsForm
                                | boardConfigForms =
                                    SafeZipper.fromList [ exampleBoardConfigForm1, exampleBoardConfigForm1, exampleBoardConfigForm3 ]
                                        |> SafeZipper.atIndex 1
                            }
                        )
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                let
                    initialState =
                        SettingsState.EditingGlobalSettings exampleSettingsFormWithThreeBoards
                in
                initialState
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfigForm1)
                    |> Expect.equal initialState
        ]



-- mapCurrentColumnsForm : Test
-- mapCurrentColumnsForm =
--     describe "mapCurrentColumnsForm"
--         [ test "does nothing if it is in the AddingBoard state" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm identity
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm)
--         , test "does nothing if it is in the AddingColumn state" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm identity
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "" "") (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm)
--         , test "does nothing if it is in the ClosingPlugin state" <|
--             \() ->
--                 SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm identity
--                     |> Expect.equal (SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm)
--         , test "does nothing if it is in the ClosingSettings state" <|
--             \() ->
--                 SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm identity
--                     |> Expect.equal (SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm)
--         , test "does nothing if it is in the DeletingBoard state" <|
--             \() ->
--                 SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm identity
--                     |> Expect.equal (SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm)
--         , test "does nothing if it is in the DeletingColumn state" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm identity
--                     |> Expect.equal (SettingsState.DeletingColumn 1 (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm)
--         , test "updates the current board if it is in the EditingBoard state" <|
--             \() ->
--                 SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig, exampleBoardConfigNoColumns ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm (always ColumnsForm.empty)
--                     |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig, exampleBoardConfigNoColumns ]) exampleBoardConfigsFormEmpty)
--         , test "does nothing if it is in the EditingGlobalSettings state" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm
--                     |> SettingsState.mapCurrentColumnsForm identity
--                     |> Expect.equal (SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) exampleBoardConfigsForm)
--         ]
--
--
-- mapColumnBeingAdded : Test
-- mapColumnBeingAdded =
--     describe "mapColumnBeingAdded"
--         [ test "does nothing if it is in the AddingBoard state" <|
--             \() ->
--                 SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always exampleNewColumn)
--                     |> Expect.equal (SettingsState.AddingBoard NewBoardForm.default Settings.default SettingsForm.empty)
--         , test "maps the board being added if it is in the AddingColumn state" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always <| NewColumnForm "a" "b")
--                     |> Expect.equal (SettingsState.AddingColumn (NewColumnForm "a" "b") Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the ClosingPlugin state" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always exampleNewColumn)
--                     |> Expect.equal (SettingsState.ClosingPlugin Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the ClosingSettings state" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always exampleNewColumn)
--                     |> Expect.equal (SettingsState.ClosingSettings Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the DeletingBoard state" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always exampleNewColumn)
--                     |> Expect.equal (SettingsState.DeletingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the DeletingColumn state" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always exampleNewColumn)
--                     |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the EditingBoard state" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always exampleNewColumn)
--                     |> Expect.equal (SettingsState.EditingBoard Settings.default SettingsForm.empty)
--         , test "does nothing if it is in the EditingGlobalSettings state" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.mapColumnBeingAdded (always exampleNewColumn)
--                     |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty)
--         ]
--
--
-- mapGlobalSettings : Test
-- mapGlobalSettings =
--     describe "mapGlobalSettings"
--         [ test "does nothing if it is in the AddingBoard state" <|
--             \() ->
--                 SettingsState.AddingBoard (NewBoardForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal GlobalSettings.default
--         , test "does nothing if it is in the AddingColumn state" <|
--             \() ->
--                 SettingsState.AddingColumn (NewColumnForm "" "") Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal GlobalSettings.default
--         , test "does nothing if it is in the ClosingPlugin state" <|
--             \() ->
--                 SettingsState.ClosingPlugin Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal GlobalSettings.default
--         , test "does nothing if it is in the ClosingSettings state" <|
--             \() ->
--                 SettingsState.ClosingSettings Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal GlobalSettings.default
--         , test "does nothing if it is in the DeletingBoard state" <|
--             \() ->
--                 SettingsState.DeletingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal GlobalSettings.default
--         , test "does nothing if it is in the DeletingColumn state" <|
--             \() ->
--                 SettingsState.DeletingColumn 1 Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal GlobalSettings.default
--         , test "does nothing if it is in the EditingBoard state" <|
--             \() ->
--                 SettingsState.EditingBoard Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal GlobalSettings.default
--         , test "updates the current board if it is in the EditingGlobalSettings state" <|
--             \() ->
--                 SettingsState.EditingGlobalSettings Settings.default SettingsForm.empty
--                     |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
--                     |> SettingsState.settings
--                     |> Settings.globalSettings
--                     |> Expect.equal exampleGlobalSettings
--         ]
--
--
--
-- HELPERS


newBoardFormBeingAdded : SettingsState -> Maybe NewBoardForm
newBoardFormBeingAdded settingsState =
    case settingsState of
        SettingsState.AddingBoard newBoardForm _ ->
            Just newBoardForm

        _ ->
            Nothing


boardConfig1 : BoardConfig
boardConfig1 =
    BoardConfig.fromConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , name = "board 1"
        , showColumnTags = False
        , showFilteredTags = False
        }


boardConfig2 : BoardConfig
boardConfig2 =
    BoardConfig.fromConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , name = "board 2"
        , showColumnTags = False
        , showFilteredTags = False
        }


boardConfig3 : BoardConfig
boardConfig3 =
    BoardConfig.fromConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Allow
        , filterScope = Filter.Both
        , name = "board 3"
        , showColumnTags = False
        , showFilteredTags = False
        }


settingsWithBoards : Settings
settingsWithBoards =
    let
        defaultSettings : Settings
        defaultSettings =
            Settings.default
    in
    { defaultSettings | boardConfigs = SafeZipper.fromList [ boardConfig1, boardConfig2, boardConfig3 ] }


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



--
--
-- exampleNewBoard : NewBoardForm
-- exampleNewBoard =
--     NewBoardForm "foo" "emptyBoard"
--
--
-- noNameNewBoard : NewBoardForm
-- noNameNewBoard =
--     NewBoardForm "" "emptyBoard"
--
--
-- exampleNewColumn : NewColumnForm
-- exampleNewColumn =
--     NewColumnForm "a name" "a type"
--
--
-- unnamedNameNewBoard : NewBoardForm
-- unnamedNameNewBoard =
--     NewBoardForm "Unnamed" "emptyBoard"
--
--
-- exampleBoardConfig : BoardConfig
-- exampleBoardConfig =
--     BoardConfig.BoardConfig
--         { columns = Columns.fromList [ Column.namedTag "foo" "bar" ]
--         , filters = []
--         , filterPolarity = Filter.Deny
--         , filterScope = Filter.SubTasksOnly
--         , showColumnTags = False
--         , showFilteredTags = True
--         , name = "Board Name"
--         }
--
--
-- exampleBoardConfigMultiColumns : BoardConfig
-- exampleBoardConfigMultiColumns =
--     BoardConfig.BoardConfig
--         { columns =
--             Columns.fromList
--                 [ Column.namedTag "named" "aTag"
--                 , Column.untagged "untagged"
--                 , Column.completed <| CompletedColumn.init "completed" 2 5
--                 ]
--         , filters = []
--         , filterPolarity = Filter.Deny
--         , filterScope = Filter.SubTasksOnly
--         , showColumnTags = False
--         , showFilteredTags = True
--         , name = "Board Name"
--         }
--
--
-- exampleBoardConfigNoColumns : BoardConfig
-- exampleBoardConfigNoColumns =
--     BoardConfig.BoardConfig
--         { columns = Columns.empty
--         , filters = []
--         , filterPolarity = Filter.Deny
--         , filterScope = Filter.SubTasksOnly
--         , showColumnTags = False
--         , showFilteredTags = True
--         , name = "Board Name"
--         }
--
--
-- exampleBoardConfigsForm : SettingsForm.Form
-- exampleBoardConfigsForm =
--     SafeZipper.fromList
--         [ BoardConfig.fromNewBoard DefaultColumnNames.default (NewBoardForm "foo" "emptyBoard")
--         , BoardConfig.fromNewBoard DefaultColumnNames.default (NewBoardForm "baz" "tagBoard")
--         ]
--         |> SafeZipper.atIndex 1
--         |> SettingsForm.init
--
--
-- exampleBoardConfigsFormEmpty : SettingsForm.Form
-- exampleBoardConfigsFormEmpty =
--     SafeZipper.fromList
--         [ BoardConfig.fromNewBoard DefaultColumnNames.default (NewBoardForm "foo" "emptyBoard")
--         , BoardConfig.fromNewBoard DefaultColumnNames.default (NewBoardForm "baz" "emptyBoard")
--         ]
--         |> SafeZipper.atIndex 1
--         |> SettingsForm.init
--
--
-- exampleGlobalSettings : GlobalSettings
-- exampleGlobalSettings =
--     { taskCompletionFormat = GlobalSettings.ObsidianTasks
--     , defaultColumnNames = DefaultColumnNames.default
--     , ignoreFileNameDates = False
--     }
--
--
-- newColumnConfigForm : SettingsState -> Maybe NewColumnForm
-- newColumnConfigForm settingsState =
--     case settingsState of
--         SettingsState.AddingColumn ncc _ _ ->
--             Just ncc
--
--         _ ->
--             Nothing
--
--
-- settingsFromBoardConfigs : List BoardConfig -> Settings
-- settingsFromBoardConfigs boardConfigs_ =
--     Settings.default
--         |> Settings.updateBoardConfigs (SafeZipper.fromList boardConfigs_)
--
--
-- settingsFromBoardConfigsWithIndex : Int -> List BoardConfig -> Settings
-- settingsFromBoardConfigsWithIndex index boardConfigs_ =
--     Settings.default
--         |> Settings.updateBoardConfigs
--             (SafeZipper.atIndex index <| SafeZipper.fromList boardConfigs_)
