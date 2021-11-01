module CardBoard exposing
    ( Settings
    , boardConfigs
    , version
    )

import CardBoardConfig
import DateBoard
import Semver
import TagBoard



-- TYPES


type alias Settings =
    { boardConfigs : List CardBoardConfig.Config
    , version : Semver.Version
    }


version : Settings -> String
version settings =
    Semver.print settings.version


boardConfigs : Settings -> { boardConfigs : List CardBoardConfig.Config }
boardConfigs settings =
    { boardConfigs = settings.boardConfigs }
