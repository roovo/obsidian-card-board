module CardBoard exposing (Config(..))

import DateBoard
import TagBoard



-- TYPES


type Config
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config
