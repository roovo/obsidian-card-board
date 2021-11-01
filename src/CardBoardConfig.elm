module CardBoardConfig exposing
    ( Config(..)
    , defaultConfig
    , isDateBoard
    , isTagBoard
    , title
    )

import DateBoard
import TagBoard



-- TYPES


type Config
    = DateBoardConfig DateBoard.Config
    | TagBoardConfig TagBoard.Config



-- INFO


isDateBoard : Config -> Bool
isDateBoard config =
    case config of
        DateBoardConfig _ ->
            True

        _ ->
            False


isTagBoard : Config -> Bool
isTagBoard config =
    case config of
        TagBoardConfig _ ->
            True

        _ ->
            False


defaultConfig : Config
defaultConfig =
    TagBoardConfig TagBoard.defaultConfig


title : Config -> String
title config =
    case config of
        DateBoardConfig dateBoardConfig ->
            dateBoardConfig.title

        TagBoardConfig tagBoardConfig ->
            tagBoardConfig.title
