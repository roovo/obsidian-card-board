module CardBoard exposing
    ( CardBoard(..)
    , Config(..)
    , columns
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



-- COLUMNS


columns : Time.Posix -> Time.Zone -> TaskList -> CardBoard -> List ( String, List TaskItem )
columns now zone taskList cardBoard =
    case cardBoard of
        Dated dateBoard ->
            DateBoard.columns now zone (dateBoard taskList)

        Tagged tagBoard ->
            TagBoard.columns (tagBoard taskList)
