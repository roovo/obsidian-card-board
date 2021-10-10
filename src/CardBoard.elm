module CardBoard exposing
    ( CardBoard(..)
    , Config(..)
    , columns
    , fromConfig
    , title
    )

import DateBoard exposing (DateBoard)
import TagBoard exposing (TagBoard)
import TaskItem exposing (TaskItem)
import TaskList exposing (TaskList)
import Time



-- TYPES


type CardBoard
    = Dated (TaskList -> DateBoard)
    | Tagged (TaskList -> TagBoard)


type Config
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config



-- SETUP


fromConfig : Config -> CardBoard
fromConfig config =
    case config of
        DateBoardConfig dateBoardConfig ->
            Dated <| DateBoard.fill dateBoardConfig

        TagBoardConfig tagBoardConfig ->
            Tagged <| TagBoard.fill tagBoardConfig



-- INFO


columns : Time.Posix -> Time.Zone -> TaskList -> CardBoard -> List ( String, List TaskItem )
columns now zone taskList cardBoard =
    case cardBoard of
        Dated dateBoard ->
            DateBoard.columns now zone (dateBoard taskList)

        Tagged tagBoard ->
            TagBoard.columns (tagBoard taskList)


title : Config -> String
title config =
    case config of
        DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title
