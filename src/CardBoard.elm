module CardBoard exposing
    ( Config(..)
    , Settings
    , boardConfigs
    , title
    , version
    )

import DateBoard
import Semver
import TagBoard



-- TYPES


type Config
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config


type alias Settings =
    { boardConfigs : List Config
    , version : Semver.Version
    }


version : Settings -> String
version settings =
    Semver.print settings.version


boardConfigs : Settings -> { boardConfigs : List Config }
boardConfigs settings =
    { boardConfigs = settings.boardConfigs }


title : Config -> String
title config =
    case config of
        DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title
