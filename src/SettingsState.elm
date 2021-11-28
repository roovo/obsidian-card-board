module SettingsState exposing
    ( CancelAction(..)
    , SettingsState(..)
    , cancelCurrentState
    , changeBoardBeingEdited
    , confirmAdd
    , confirmDelete
    , forNoConfig
    , mapBoardBeingAdded
    , mapBoardBeingEdited
    , moveToAdd
    , moveToDelete
    , startEditing
    )

import BoardConfig exposing (BoardConfig)
import InteropPorts
import SafeZipper exposing (SafeZipper)



-- TYPES


type SettingsState
    = AddingBoard (SafeZipper BoardConfig) BoardConfig
    | DeletingBoard (SafeZipper BoardConfig)
    | EditingBoard (SafeZipper BoardConfig)


type CancelAction
    = ExitWithConfig (SafeZipper BoardConfig)
    | ExitWithNoConfig
    | SetToState



-- CONSTRUCTION


forNoConfig : SettingsState
forNoConfig =
    AddingBoard SafeZipper.empty BoardConfig.default


startEditing : SafeZipper BoardConfig -> SettingsState
startEditing boardConfigs =
    EditingBoard boardConfigs



-- MAPPING


mapBoardBeingAdded : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingAdded fn =
    fromAddingConfigTo (\cs c -> AddingBoard cs (fn c))


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsState -> SettingsState
mapBoardBeingEdited fn =
    fromEditingConfigTo (\c -> EditingBoard (SafeZipper.mapCurrent fn c))



-- STATE TRANSITIONS


cancelCurrentState : SettingsState -> ( SettingsState, CancelAction )
cancelCurrentState settingsState =
    case settingsState of
        AddingBoard configs _ ->
            ( EditingBoard configs, exitIfNoConfigs configs )

        DeletingBoard configs ->
            ( EditingBoard configs, SetToState )

        EditingBoard configs ->
            ( EditingBoard configs, ExitWithConfig configs )


exitIfNoConfigs : SafeZipper BoardConfig -> CancelAction
exitIfNoConfigs boardConfigs =
    if SafeZipper.length boardConfigs == 0 then
        ExitWithNoConfig

    else
        SetToState


changeBoardBeingEdited : Int -> SettingsState -> SettingsState
changeBoardBeingEdited index =
    fromEditingConfigTo (EditingBoard << SafeZipper.atIndex index)


confirmAdd : SettingsState -> SettingsState
confirmAdd =
    let
        configWithNew cs c =
            SafeZipper.add c cs
                |> SafeZipper.last
    in
    fromAddingConfigTo (\cs c -> EditingBoard <| configWithNew cs c)


confirmDelete : SettingsState -> SettingsState
confirmDelete settingsState =
    let
        foo : SafeZipper BoardConfig -> SettingsState
        foo cs =
            if SafeZipper.length cs == 1 then
                forNoConfig

            else
                EditingBoard <| SafeZipper.deleteCurrent cs
    in
    fromDeletingConfigTo foo settingsState


moveToAdd : SettingsState -> SettingsState
moveToAdd =
    fromEditingConfigTo (\cs -> AddingBoard cs BoardConfig.default)


moveToDelete : SettingsState -> SettingsState
moveToDelete =
    fromEditingConfigTo DeletingBoard



-- HELPERS


fromAddingConfigTo : (SafeZipper BoardConfig -> BoardConfig -> SettingsState) -> SettingsState -> SettingsState
fromAddingConfigTo fn existingState =
    case existingState of
        AddingBoard cs c ->
            fn cs c

        _ ->
            existingState


fromDeletingConfigTo : (SafeZipper BoardConfig -> SettingsState) -> SettingsState -> SettingsState
fromDeletingConfigTo fn existingState =
    case existingState of
        DeletingBoard cs ->
            fn cs

        _ ->
            existingState


fromEditingConfigTo : (SafeZipper BoardConfig -> SettingsState) -> SettingsState -> SettingsState
fromEditingConfigTo fn existingState =
    case existingState of
        EditingBoard cs ->
            fn cs

        _ ->
            existingState
