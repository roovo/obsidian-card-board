module Column.Untagged exposing
    ( UntaggedColumn
    , init
    , isEnabled
    , name
    , taskList
    )

import ColumnNames exposing (ColumnNames)
import TagBoard
import TaskList exposing (TaskList)



-- TYPES


type UntaggedColumn
    = UntaggedColumn Config


type alias Config =
    { enabled : Bool
    , name : String
    , taskList : TaskList
    }



-- CONSTRUCTION


init : TagBoard.Config -> ColumnNames -> UntaggedColumn
init tagboardConfig columnNames =
    UntaggedColumn
        { enabled = tagboardConfig.includeUntagged
        , name = ColumnNames.nameFor "untagged" columnNames
        , taskList = TaskList.empty
        }



-- INFO


isEnabled : UntaggedColumn -> Bool
isEnabled =
    .enabled << config


name : UntaggedColumn -> String
name =
    .name << config


taskList : UntaggedColumn -> TaskList
taskList =
    .taskList << config



-- PRIVATE


config : UntaggedColumn -> Config
config (UntaggedColumn c) =
    c
