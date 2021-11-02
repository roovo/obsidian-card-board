module Model exposing
    ( EditState(..)
    , Model
    , State(..)
    , default
    , forceAddWhenNoBoards
    )

import BoardConfig exposing (BoardConfig)
import SafeZipper exposing (SafeZipper)
import TaskList exposing (TaskList)
import Time
import TimeWithZone exposing (TimeWithZone)



-- TYPES


type alias Model =
    { boardConfigs : SafeZipper BoardConfig
    , configBeingEdited : EditState
    , taskList : State TaskList
    , timeWithZone : TimeWithZone
    }


type EditState
    = Adding (SafeZipper BoardConfig) BoardConfig
    | Deleting (SafeZipper BoardConfig)
    | Editing (SafeZipper BoardConfig)
    | NotEditing


type State a
    = Waiting
    | Loading a
    | Loaded a


default : Model
default =
    { boardConfigs = SafeZipper.fromList []
    , configBeingEdited = Adding (SafeZipper.fromList []) BoardConfig.default
    , taskList = Waiting
    , timeWithZone =
        { now = Time.millisToPosix 0
        , zone = Time.customZone 0 []
        }
    }


forceAddWhenNoBoards : SafeZipper BoardConfig -> Model -> Model
forceAddWhenNoBoards config model =
    if SafeZipper.length config == 0 then
        { model | configBeingEdited = Adding config BoardConfig.default }

    else
        model
