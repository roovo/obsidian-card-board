module CardBoard exposing
    ( Config(..)
    , columns
    , title
    )

import DateBoard
import TagBoard
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type Config
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config



-- INFO


columns : Time.Posix -> Time.Zone -> TaskList -> Config -> List ( String, List TaskItem )
columns now zone taskList config =
    case config of
        DateBoardConfig dateBoardConfig ->
            DateBoard.columns now zone dateBoardConfig taskList

        TagBoardConfig tagBoardConfig ->
            TagBoard.columns tagBoardConfig taskList


title : Config -> String
title config =
    case config of
        DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title
