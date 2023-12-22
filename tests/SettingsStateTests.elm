module SettingsStateTests exposing (suite)

import BoardConfig exposing (BoardConfig)
import Column
import Columns
import DefaultColumnNames
import Expect
import Filter
import GlobalSettings exposing (GlobalSettings)
import NewBoardConfig exposing (NewBoardConfig)
import NewColumnConfig exposing (NewColumnConfig)
import SafeZipper
import Settings exposing (Settings)
import SettingsState exposing (SettingsState)
import Test exposing (..)


suite : Test
suite =
    concat
        [ addBoardRequested
        , addColumnRequested
        , boardConfigs
        , cancelCurrentState
        , confirmAddBoard
        , confirmAddColumn
        , confirmDelete
        , deleteBoardRequested
        , deleteColumnRequested
        , editBoardAt
        , editGlobalSettings
        , init
        , mapBoardBeingAdded
        , mapBoardBeingEdited
        , mapColumnBeingAdded
        , mapGlobalSettings
        ]


addBoardRequested : Test
addBoardRequested =
    describe "addBoardRequested"
        [ test "does nothing if already in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "AddingColumn -> AddingBoard" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "ClosingPlugin -> AddingBoard" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "ClosingSettings -> AddingBoard" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "DeletingBoard -> AddingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "DeletingColumn -> AddingBoard" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "EditingBoard -> AddingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "EditingGlobalSettings -> AddingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.addBoardRequested
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        ]


addColumnRequested : Test
addColumnRequested =
    describe "addColumnRequested"
        [ test "AddingBoard -> AddingColumn" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        , test "the NewColumnConfig defaults to the first dropdown item" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.addColumnRequested
                    |> newColumnConfig
                    |> Maybe.map .columnType
                    |> Expect.equal (Just "completed")
        , test "does nothing if already in AddingColumn state" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        , test "ClosingPlugin -> AddingColumn" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        , test "ClosingSettings -> AddingColumn" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        , test "DeletingBoard -> AddingColumn" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        , test "DeletingColumn -> AddingColumn" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        , test "EditingBoard -> AddingColumn" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        , test "EditingGlobalSettings -> AddingColumn" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.addColumnRequested
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "completed") Settings.default [])
        ]


boardConfigs : Test
boardConfigs =
    describe "boardConfigs"
        [ test "returns the configs (apart from the one being added) if in AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        , test "returns the configs if in AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        , test "returns the configs if in ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        , test "returns the configs if in ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        , test "returns the configs if in DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        , test "returns the configs if in DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 1 (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        , test "returns the configs if in EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        , test "returns the configs if in EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.boardConfigs
                    |> Expect.equal (SafeZipper.fromList [ exampleBoardConfig ])
        ]


cancelCurrentState : Test
cancelCurrentState =
    describe "cancelCurrentState"
        [ test "AddingBoard -> ClosingPlugin if the board list is empty" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default [])
        , test "AddingBoard -> EditingBoard if the board list is NOT empty" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) [])
        , test "AddingColumn -> EditingBoard" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) [])
        , test "ClosingPlugin -> ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default [])
        , test "ClosingSettings -> ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "DeletingColumn -> EditingBoard" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "EditingBoard -> ClosingSettings" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        , test "EditingGlobalSettings -> ClosingSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.cancelCurrentState
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        ]


confirmAddBoard : Test
confirmAddBoard =
    describe "confirmAddBoard"
        [ test "AddingBoard -> EditingBoard focussed on the new board which is on the end" <|
            \() ->
                SettingsState.AddingBoard exampleNewBoardConfig
                    (settingsFromBoardConfigs [ exampleBoardConfig ])
                    []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal
                        (SettingsState.EditingBoard
                            (settingsFromBoardConfigsWithIndex 1
                                [ exampleBoardConfig
                                , BoardConfig.fromNewBoardConfig DefaultColumnNames.default exampleNewBoardConfig
                                ]
                            )
                            []
                        )
        , test "AddingBoard -> EditingBoard changes blank name to Unnamed" <|
            \() ->
                SettingsState.AddingBoard noNameNewBoardConfig
                    (settingsFromBoardConfigs [ exampleBoardConfig ])
                    []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal
                        (SettingsState.EditingBoard
                            (settingsFromBoardConfigsWithIndex 1
                                [ exampleBoardConfig
                                , BoardConfig.fromNewBoardConfig DefaultColumnNames.default unnamedNameNewBoardConfig
                                ]
                            )
                            []
                        )
        , test "does nothing if AddingColumn" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default [])
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default [])
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "does nothing if DeletingColumn" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default [])
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.confirmAddBoard DefaultColumnNames.default
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        ]


confirmAddColumn : Test
confirmAddColumn =
    describe "confirmAddColumn"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "AddingColumn -> EditingBoard" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default [])
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        , test "does nothing if DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "does nothing if DeletingColumn" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default [])
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.confirmAddColumn DefaultColumnNames.default
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        ]


confirmDelete : Test
confirmDelete =
    describe "confirmDelete"
        [ test "does nothing if AddingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "does nothing if AddingColumn" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default [])
        , test "does nothing if ClosingPlugin" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default [])
        , test "does nothing if ClosingSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        , test "deletes the current board and -> Adding if DeletingBoard and there is ONLY one board" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "deletes the current board and -> EditingBoard if DeletingBoard and there is MORE THAN one board" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig, exampleBoardConfigNoColumns ]) []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfigNoColumns ]) [])
        , test "deletes the current column and -> EditingBoard if DeletingColumn" <|
            \() ->
                SettingsState.DeletingColumn 0 (settingsFromBoardConfigs [ exampleBoardConfig ]) []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfigNoColumns ]) [])
        , test "does nothing if EditingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "does nothing if EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.confirmDelete
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        ]


deleteBoardRequested : Test
deleteBoardRequested =
    describe "deleteBoardRequested"
        [ test "AddingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "AddingColumn -> DeletingBoard" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "ClosingPlugin -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "ClosingSettings -> DeletingBoard" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "does nothing if already DeletingBoard" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "DeletingColumn -> DeletingBoard" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "EditingBoard -> DeletingBoard" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "EditingGlobalSettings -> DeletingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.deleteBoardRequested
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        ]


deleteColumnRequested : Test
deleteColumnRequested =
    describe "deleteColumnRequested"
        [ test "AddingBoard -> DeletingColumn" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        , test "AddingColumn -> DeletingColumn" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        , test "ClosingPlugin -> DeletingColumn" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        , test "ClosingSettings -> DeletingColumn" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        , test "DeletingBoard -> DeletingColumn" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        , test "ensure deleting the given column if already in DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 2 Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        , test "EditingBoard -> DeletingColumn" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        , test "EditingGlobalSettings -> DeletingColumn" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.deleteColumnRequested 3
                    |> Expect.equal (SettingsState.DeletingColumn 3 Settings.default [])
        ]


editBoardAt : Test
editBoardAt =
    describe "editBoardAt"
        [ test "AddingBoard -> EditingBoard" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "AddingColumn -> EditingBoard" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "ClosingPlugin -> EditingBoard" <|
            \() ->
                SettingsState.ClosingPlugin (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "ClosingSettings -> EditingBoard" <|
            \() ->
                SettingsState.ClosingSettings (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "DeletingBoard -> EditingBoard" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "DeletingColumn -> EditingBoard" <|
            \() ->
                SettingsState.DeletingColumn 1 (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "EditingBoard -> EditingBoard (switched)" <|
            \() ->
                SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "EditingGlobalSettings -> EditingBoard" <|
            \() ->
                SettingsState.EditingGlobalSettings (settingsFromBoardConfigsWithIndex 0 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.editBoardAt 1
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigsWithIndex 1 [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        ]


editGlobalSettings : Test
editGlobalSettings =
    describe "editGlobalSettings"
        [ test "AddingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        , test "AddingColumn -> EditingGlobalSettings" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        , test "ClosingPlugin -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        , test "ClosingSettings -> EditingGlobalSettings" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        , test "DeletingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        , test "DeletingColumn -> EditingGlobalSettings" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        , test "EditingBoard -> EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        , test "does nothing to EditingGlobalSettings" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.editGlobalSettings
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        ]


init : Test
init =
    describe "init"
        [ test "returns AddingBoard with the default BoardConfig if there are no existing board configs" <|
            \() ->
                SettingsState.init Settings.default
                    |> Expect.equal
                        (SettingsState.AddingBoard
                            NewBoardConfig.default
                            Settings.default
                            []
                        )
        , test "returns EditingBoard boardConfig" <|
            \() ->
                SettingsState.init (settingsFromBoardConfigs [ exampleBoardConfigNoColumns ])
                    |> Expect.equal
                        (SettingsState.EditingBoard
                            (settingsFromBoardConfigs [ exampleBoardConfigNoColumns ])
                            []
                        )
        ]


mapBoardBeingAdded : Test
mapBoardBeingAdded =
    describe "mapBoardBeingAdded"
        [ test "maps the board being added if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.AddingBoard exampleNewBoardConfig Settings.default [])
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default [])
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default [])
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default [])
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.mapBoardBeingAdded (always exampleNewBoardConfig)
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        ]


mapBoardBeingEdited : Test
mapBoardBeingEdited =
    describe "mapBoardBeingEdited"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "" "") (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.ClosingPlugin (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.ClosingSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.DeletingBoard (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 1 (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.DeletingColumn 1 (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        , test "updates the current board if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.EditingBoard (settingsFromBoardConfigs [ exampleBoardConfig, exampleBoardConfig ]) [])
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) []
                    |> SettingsState.mapBoardBeingEdited (always exampleBoardConfig)
                    |> Expect.equal (SettingsState.EditingGlobalSettings (settingsFromBoardConfigs [ exampleBoardConfigNoColumns, exampleBoardConfig ]) [])
        ]


mapColumnBeingAdded : Test
mapColumnBeingAdded =
    describe "mapColumnBeingAdded"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard NewBoardConfig.default Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always exampleNewColumnConfig)
                    |> Expect.equal (SettingsState.AddingBoard NewBoardConfig.default Settings.default [])
        , test "maps the board being added if it is in the AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always <| NewColumnConfig "a" "b")
                    |> Expect.equal (SettingsState.AddingColumn (NewColumnConfig "a" "b") Settings.default [])
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always exampleNewColumnConfig)
                    |> Expect.equal (SettingsState.ClosingPlugin Settings.default [])
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always exampleNewColumnConfig)
                    |> Expect.equal (SettingsState.ClosingSettings Settings.default [])
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always exampleNewColumnConfig)
                    |> Expect.equal (SettingsState.DeletingBoard Settings.default [])
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always exampleNewColumnConfig)
                    |> Expect.equal (SettingsState.DeletingColumn 1 Settings.default [])
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always exampleNewColumnConfig)
                    |> Expect.equal (SettingsState.EditingBoard Settings.default [])
        , test "does nothing if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.mapColumnBeingAdded (always exampleNewColumnConfig)
                    |> Expect.equal (SettingsState.EditingGlobalSettings Settings.default [])
        ]


mapGlobalSettings : Test
mapGlobalSettings =
    describe "mapGlobalSettings"
        [ test "does nothing if it is in the AddingBoard state" <|
            \() ->
                SettingsState.AddingBoard (NewBoardConfig "" "") Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the AddingColumn state" <|
            \() ->
                SettingsState.AddingColumn (NewColumnConfig "" "") Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the ClosingPlugin state" <|
            \() ->
                SettingsState.ClosingPlugin Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the ClosingSettings state" <|
            \() ->
                SettingsState.ClosingSettings Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the DeletingBoard state" <|
            \() ->
                SettingsState.DeletingBoard Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the DeletingColumn state" <|
            \() ->
                SettingsState.DeletingColumn 1 Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "does nothing if it is in the EditingBoard state" <|
            \() ->
                SettingsState.EditingBoard Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal GlobalSettings.default
        , test "updates the current board if it is in the EditingGlobalSettings state" <|
            \() ->
                SettingsState.EditingGlobalSettings Settings.default []
                    |> SettingsState.mapGlobalSettings (always exampleGlobalSettings)
                    |> SettingsState.settings
                    |> Settings.globalSettings
                    |> Expect.equal exampleGlobalSettings
        ]



-- HELPERS


exampleNewBoardConfig : NewBoardConfig
exampleNewBoardConfig =
    NewBoardConfig "foo" "emptyBoard"


noNameNewBoardConfig : NewBoardConfig
noNameNewBoardConfig =
    NewBoardConfig "" "emptyBoard"


exampleNewColumnConfig : NewColumnConfig
exampleNewColumnConfig =
    NewColumnConfig "a name" "a type"


unnamedNameNewBoardConfig : NewBoardConfig
unnamedNameNewBoardConfig =
    NewBoardConfig "Unnamed" "emptyBoard"


exampleBoardConfig : BoardConfig
exampleBoardConfig =
    BoardConfig.BoardConfig
        { columns = Columns.fromList [ Column.namedTag "foo" "bar" ]
        , filters = []
        , filterPolarity = Filter.Deny
        , filterScope = Filter.SubTasksOnly
        , showColumnTags = False
        , showFilteredTags = True
        , name = "Board Name"
        }


exampleBoardConfigNoColumns : BoardConfig
exampleBoardConfigNoColumns =
    BoardConfig.BoardConfig
        { columns = Columns.empty
        , filters = []
        , filterPolarity = Filter.Deny
        , filterScope = Filter.SubTasksOnly
        , showColumnTags = False
        , showFilteredTags = True
        , name = "Board Name"
        }


exampleGlobalSettings : GlobalSettings
exampleGlobalSettings =
    { taskCompletionFormat = GlobalSettings.ObsidianTasks
    , defaultColumnNames = DefaultColumnNames.default
    , ignoreFileNameDates = False
    }


newColumnConfig : SettingsState -> Maybe NewColumnConfig
newColumnConfig settingsState =
    case settingsState of
        SettingsState.AddingColumn ncc _ _ ->
            Just ncc

        _ ->
            Nothing


settingsFromBoardConfigs : List BoardConfig -> Settings
settingsFromBoardConfigs boardConfigs_ =
    Settings.default
        |> Settings.updateBoardConfigs (SafeZipper.fromList boardConfigs_)


settingsFromBoardConfigsWithIndex : Int -> List BoardConfig -> Settings
settingsFromBoardConfigsWithIndex index boardConfigs_ =
    Settings.default
        |> Settings.updateBoardConfigs
            (SafeZipper.atIndex index <| SafeZipper.fromList boardConfigs_)
