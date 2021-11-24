module SettingsEditState exposing
    ( CancelAction(..)
    , SettingsEditState(..)
    , cancelCurrentState
    , changeBoardBeingEdited
    , confirmAdd
    , confirmDelete
    , default
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


type SettingsEditState
    = AddingBoard (SafeZipper BoardConfig) BoardConfig
    | DeletingBoard (SafeZipper BoardConfig)
    | EditingBoard (SafeZipper BoardConfig)
    | NotBeingEdited


type CancelAction
    = ExitWithConfig (SafeZipper BoardConfig)
    | ExitWithNoConfig
    | SetToState



-- CONSTRUCTION


default : SettingsEditState
default =
    NotBeingEdited


forNoConfig : SettingsEditState
forNoConfig =
    AddingBoard SafeZipper.empty BoardConfig.default


startEditing : SafeZipper BoardConfig -> SettingsEditState
startEditing boardConfigs =
    EditingBoard boardConfigs



-- MAPPING


mapBoardBeingAdded : (BoardConfig -> BoardConfig) -> SettingsEditState -> SettingsEditState
mapBoardBeingAdded fn =
    fromAddingConfigTo (\cs c -> AddingBoard cs (fn c))


mapBoardBeingEdited : (BoardConfig -> BoardConfig) -> SettingsEditState -> SettingsEditState
mapBoardBeingEdited fn =
    fromEditingConfigTo (\c -> EditingBoard (SafeZipper.mapCurrent fn c))



-- STATE TRANSITIONS


cancelCurrentState : SettingsEditState -> ( SettingsEditState, CancelAction )
cancelCurrentState settingsEditState =
    case settingsEditState of
        AddingBoard configs _ ->
            ( EditingBoard configs, exitIfNoConfigs configs )

        DeletingBoard configs ->
            ( EditingBoard configs, SetToState )

        EditingBoard configs ->
            ( NotBeingEdited, ExitWithConfig configs )

        NotBeingEdited ->
            ( NotBeingEdited, SetToState )


exitIfNoConfigs : SafeZipper BoardConfig -> CancelAction
exitIfNoConfigs boardConfigs =
    if SafeZipper.length boardConfigs == 0 then
        ExitWithNoConfig

    else
        SetToState


changeBoardBeingEdited : Int -> SettingsEditState -> SettingsEditState
changeBoardBeingEdited index =
    fromEditingConfigTo (EditingBoard << SafeZipper.atIndex index)


confirmAdd : SettingsEditState -> SettingsEditState
confirmAdd =
    let
        configWithNew cs c =
            SafeZipper.add c cs
                |> SafeZipper.last
    in
    fromAddingConfigTo (\cs c -> EditingBoard <| configWithNew cs c)


confirmDelete : SettingsEditState -> SettingsEditState
confirmDelete settingsEditState =
    let
        foo : SafeZipper BoardConfig -> SettingsEditState
        foo cs =
            if SafeZipper.length cs == 1 then
                forNoConfig

            else
                EditingBoard <| SafeZipper.deleteCurrent cs
    in
    fromDeletingConfigTo foo settingsEditState


moveToAdd : SettingsEditState -> SettingsEditState
moveToAdd =
    fromEditingConfigTo (\cs -> AddingBoard cs BoardConfig.default)


moveToDelete : SettingsEditState -> SettingsEditState
moveToDelete =
    fromEditingConfigTo DeletingBoard



-- HELPERS


fromAddingConfigTo : (SafeZipper BoardConfig -> BoardConfig -> SettingsEditState) -> SettingsEditState -> SettingsEditState
fromAddingConfigTo fn existingState =
    case existingState of
        AddingBoard cs c ->
            fn cs c

        _ ->
            existingState


fromDeletingConfigTo : (SafeZipper BoardConfig -> SettingsEditState) -> SettingsEditState -> SettingsEditState
fromDeletingConfigTo fn existingState =
    case existingState of
        DeletingBoard cs ->
            fn cs

        _ ->
            existingState


fromEditingConfigTo : (SafeZipper BoardConfig -> SettingsEditState) -> SettingsEditState -> SettingsEditState
fromEditingConfigTo fn existingState =
    case existingState of
        EditingBoard cs ->
            fn cs

        _ ->
            existingState
